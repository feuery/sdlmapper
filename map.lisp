(defpackage :qmapper.map
  (:use :common-lisp
	:qmapper.obj
	:multimethods
	:cl-arrows
	:cl-fad
	:qmapper.std
	:qmapper.engine
        :qmapper.tileset
	:qmapper.animatedsprite
	:qmapper.script
	:qmapper.sprite
	:qmapper.layer
	:qmapper.root
	:qmapper.export
	:qmapper.tile)
  ;(:import-from :qmapper.export :clear-lisp-drawingqueue :add-lambda-to-drawingqueue)
					;(:import-from :fset :size :convert)
  (:export :init-map :map-findNearest :animatedsprites :sprites :push-layer :set-tile-at :layers :qmap :map-id :map-name :id :map-layers :id))

(in-package :qmapper.map)

(defun-export! find-nearest  (x y lst)
  (->> lst
       (mapcar (lambda (sprite)
		 (let ((position (get-pos sprite)))
		   (when position
		     (destructuring-bind (sprites-x sprites-y) position
		       
		       (list (distance sprites-x sprites-y
				       x y)
			     sprite))))))
       (sort-by #'first)
       (first)
       (last)))


(defun boolp (b &rest rst)
  (typep b 'boolean))

(defparameter *map-init-counter* 0)

(defclass* map
  (name (format nil "Map ~a" (incf *map-init-counter*)))
   (layers '())
   (id (random 99999))
   (sprites '())
   (animatedSprites '())
   (hit-layer '())
   (on-load-scripts nil)
   (on-unload-scripts nil)
   (has-gravity? nil)) ;;validator  #'boolp))

(defun init-map (map &key layer-w layer-h layer-count)
  (unless (equalp layer-w :DEMO)
    (with-slots* (layers hit-layer) map
      (setf layers (->> (range layer-count)
			(mapcar #'dec)
			(mapcar (lambda (index)
				  (make-layer :name (format nil "Layer ~a" index)
					      :tiles (make-tiles layer-w layer-h))))))
      (setf hit-layer (make-hitlayer layer-w layer-h)))))

(defun push-layer (map)
  (with-slots* (layers) map
    (let ((w (map-width map))
	  (h (map-height map)))
      ;; TODO jatka tästä paskesta 
      (setf layers (concatenate 'list layers (list (make-layer :name (format nil "Layer ~a" (length layers))
							       :tiles (make-tiles w h))))))))



(defun set-tile-at (map layer x y tile)
  (assert (numberp layer))
  (handler-case 
      (with-slots* (layers) map
		   (let ((layer-obj (nth layer layers)))
		     (setf (nth layer layers)
			   (with-slots* (qmapper.layer:tiles) layer-obj
					(setf (nth x qmapper.layer:tiles)
					      (let ((inner-list (nth x tiles)))
						(format t "inner-list: ~a~%and tile~a~%" inner-list tile)
						(setf (nth y inner-list) tile)
						inner-list))))))
    (error (c)
      (format t "error: ~a~%" c))))
      


(defun map-findNearest (map mouse-x mouse-y)
  ;; Let's search the nearest animatedsprite or sprite
  (let* ((lst  (->> (map-sprites map)
		    (concatenate 'list 
				 (map-animatedSprites map)))))
    (car (find-nearest mouse-x mouse-y lst))))

(defun get-layer (doc map index)
  (let ((layer-list (map-layers map)))
    (get-prop (root-layers doc) (get-prop layer-list index))))

;; epicly copypasted from https://groups.google.com/d/msg/comp.lang.lisp/QWk0uDw2Fd0/UZ1nKv9hd0QJ
(defun mapcar-with-index (function list &rest more-lists)
  (let ((index -1))
    (apply #'mapcar (lambda (&rest args)
                      (incf index)
                      (apply function (append args (list index))))
           (append (list list) more-lists))))

;; (defun set-image-subobject (dst tile subtile)
;;   (funcall set-img-subobj dst (tile-gl-key tile) (tile-gl-key subtile)))

(defun draw-colored-rect (renderer x y r g b a)
  (multiple-value-bind (old-r old-g old-b old-a) (sdl2:get-render-draw-color renderer)
    (sdl2:set-render-draw-color renderer r g b a)
    (sdl2:render-fill-rect renderer (sdl2:make-rect x y 50 50))
    (sdl2:set-render-draw-color renderer old-r old-g old-b old-a)))

(defun save-and-load-tmp! (contents)
  "Saves the parameter as a file in /tmp and tries to (load) it. This tries to hack around CL's eval not knowing where just-imported symbols should be imported from"
  (with-open-temporary-file (s :direction :io :keep t)
    (format s "~a" contents)
    (load (pathname s))
    (format t "Loaded ~a~%" (pathname s))))
  

(defun-export! set-engine-chosen-map! (map-id)
  (let* ((engine-doc qmapper.root:*engine-document*)
	 (map (get-prop-in engine-doc (list "MAPS" map-id)))
	 (gravity? (map-has-gravity? map))
	 ;;(script-ids (map-scripts-to-run map))
	 (scripts (fset:empty-map) ;;  (fset:image (lambda (id)
			      ;; 	(get-prop-in engine-doc (list "SCRIPTS" id)))
			      ;; script-ids)
		  ))
    (stop-gravity-loop!)
    ;; (format t "map: ~a~%" map)
    (format t "stopped gravity, restarting it? ~a~%" gravity?)
    (if gravity?
	(start-gravity-loop!))
    (dolist (script (convert 'list scripts))
      (let ((contents (script-contents script)))
	(save-and-load-tmp! contents)))
    
    (set-map-renderer-fn :ENGINE map-id)
    (set-engine-doc (set-root-chosenmap! engine-doc map-id))))

(defun-export! map-width (map)
  (with-slots* (layers) map :read-only
    (layer-width (car layers))))

(defun-export! map-height (map)
  (with-slots* (layers) map :read-only
    (layer-height (car layers))))

(defun get-tile-at (layer x y)
  (with-slots* (qmapper.layer:tiles) layer :read-only
    (get-in qmapper.layer:tiles (list x y))))
  
(defun fetch-tile-from-tileset (tileset x y)
  (when tileset
    (let ((tiles (tileset-tiles tileset)))
      (get-in tiles (list x y)))))

(defun-export! hit-tool-chosen? ()
  (equalp (root-chosentool *document*) :HITDATA-TOOL))

(defmultimethod qmapper.obj:draw "map" (map args)
  (let ((renderer (fset:lookup args "RENDERER"))
	(dst (fset:lookup args "DST"))
	(x (or (fset:lookup args "X") 0))
	(y (or (fset:lookup args "Y") 0)))

    ;;(declare (optimize debug))
    (if-let (*local-document* (if (equalp dst :ENGINE)
				  *engine-document*
				  qmapper.root:*document*))
      (let* ((hitdata (map-hit-layer map))
	     (w (map-width map))
	     (h (map-height map))
	     (x-coords (mapcar #'dec (range w)))
	     (y-coords (mapcar #'dec (range h)))
	     (layers (->> (map-layers map)
			  (remove-if-not #'layer-visible))) ;;old layer-list
	     (sprites (map-sprites map))
	     (animations (map-animatedsprites map)))
	
	(->> layers
	     (mapcar-with-index (lambda (layer index)
				  ;;(let ((layer (get-prop (root-layers root) (get-prop layer-list l))))
				  (let ((coord-pairs (pairs x-coords y-coords)))
				    (dolist (pair coord-pairs)
				      ;;(format t "Drawing tile at ~a~%" pair)
				      (let ((x (car pair))
					    (y (cadr pair)))
					
					(if-let (tile (get-tile-at layer x y))
					  (progn
					    (let* ((rotation (tile-rotation tile))
						   (sprite (tile-sprite tile))
						   (tile (if sprite
							     tile
							     (if (tile-tileset tile)
								 (when-let (tile (fetch-tile-from-tileset (nth (tile-tileset tile) (root-tilesets *local-document*))
													  (tile-x tile)
													  (tile-y tile)))
								   tile))))
						   ;; (subtile (if (not (zerop index))
						   ;; 		    (let ((subtile (get-tile-at map (nth (dec index) final-l-coords) x y)))
						   ;; 		      (if (valid-gl-key? (tile-gl-key subtile))
						   ;; 			  subtile
						   ;; 			  (fetch-tile-from-tileset root
						   ;; 						   (tile-tileset subtile)
						   ;; 						   (tile-x subtile)
						   ;; 						   (tile-y subtile))))))
						   )
					      
					      (when tile
						(draw tile (fset:map 
							    ("ROTATION" rotation)
							    ("RENDERER" renderer)
							    ("X" (* x 50))
							    ("Y" (* y 50))
							    ("OPACITY" (layer-opacity layer))))))))))))))
	(if (hit-tool-chosen?)
	    (let ((pairs (pairs x-coords y-coords)))
	      (dolist (pair pairs)
		(let* ((x (car pair))
		       (y (cadr pair))
		       (hit-tile (get-prop-in hitdata (list x y))))
		  
		  (draw-colored-rect renderer 
				     (* x 50)
				     (* y 50)
				     (if hit-tile
					 0
					 255)
				     (if hit-tile
					 255 0)
				     0
				     70)))))

	(dolist (sprite sprites)
;;	  (format t "Rendering sprite ~a~%" sprite)
	  (draw sprite (fset:map ("RENDERER" renderer))))
      	
	;;(format t "animations: ~a~%" (clean-hashmaps animations))
	(dolist (animation animations)
	  (draw (clean-hashmaps animation) (fset:map ("RENDERER" renderer))))))))

(defun-export! select-map-layer (root map-id layer-id)
  ;;(clear-lisp-dq :MAP)
  ;;(set-map-renderer-fn :MAP map-id)
  
  (-> root
      (set-root-chosenlayerind! layer-id)
      (set-root-chosenmap! map-id)))

(defun selected-map (root)
  (let ((selected-map-id (root-chosenmap root)))
    (get-prop-in root (list "MAPS" selected-map-id))))

(defun-export! resize-selected-map (root new-w new-h)
  (let* ((selected-map (selected-map root))
	 (layer-ids (map-layers selected-map)))
    (fset:reduce (lambda (root layer-id)
		   (update-prop-in root (list "LAYERS" layer-id) (lambda (layer)
								   (resize-layer layer new-w new-h))))
		 layer-ids :initial-value root)))

(defun-export! selected-map-width (root)
  (true-map-width (selected-map root)))

(defun-export! selected-map-height (root)
  (true-map-height (selected-map root)))
