(defpackage :qmapper.map
  (:use :common-lisp
	:qmapper.obj
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
		 (destructuring-bind (sprites-x sprites-y) (get-pos sprite)
		   
		   (list (distance sprites-x sprites-y
				   x y)
			 sprite))))
       (sort-by #'first)
       (first)
       (last)))

;; tää ei toimi
(defun-export! invert-hit-tile (map x y)
  ;; get-prop-in is a bit stupid and coerces nils to #[ ], of which #'not doesn't understand anything
  (update-prop-in map (list "HIT-LAYER" x y) (lambda (hit-obj)
					       (if (equalp hit-obj (fset:seq))
						   t
						   (not hit-obj)))))

;; tääkään ei toimi
(defun-export! invert-hit-tile-in-chosen-map (root x y)
  (update-prop-in root (list "MAPS" (root-chosenmap root))
		  (lambda (map)
		    (invert-hit-tile map x y))))

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
   (scripts-to-run '())
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
					(setf qmapper.layer:tiles
					      (let ((inner-list (nth x tiles)))
						(setf (nth y inner-list) tile)
						innter-list))))))
    (error (c)
      (format t "error: ~a~%" c))))
      


(defun map-findNearest (map mouse-x mouse-y)
  ;; Let's search the nearest animatedsprite or sprite
  (let* ((lst  (->> (map-sprites map)
		    (concatenate 'list 
				 (map-animatedSprites map)))))
    (car (find-nearest mouse-x mouse-y lst))))

      

;; (defun-export! make-map-with-layers (doc name w h layer-count)
;;   (let* ((layers (repeatedly (lambda (i)
;; 			       (let ((l 
;; 				       (make-Layer :name (str (prin1-to-string i) "th layer")
;; 						   :tiles
;; 						   (make-tiles w h))))
;; 					;(format t "making layer ~a ~%" l)
;; 				 l)) layer-count))
;; 	 (ids (mapcar (lambda (l) (get-prop l "ID")) layers))
;; 	 (map (make-map :name name
;; 			:layers ids
;; 			:sprites '()
;; 			:animatedSprites '()
;; 			:hit-layer (make-hitlayer w h)
;; 			:scripts-to-run '())))
;;     (-> (set-root-layers! doc
;; 			  (reduce (lambda (all-layers layer)
;; 				    (set-prop all-layers (get-prop layer "ID") layer)) layers :initial-value (root-layers doc)))
;; 	(set-root-maps! (set-prop (root-maps doc) (get-prop map "ID") map))
;; 	(set-root-chosenmap! (get-prop map "ID")))))

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

(defun draw-colored-rect (x y r g b a)
  (error 'not-implemented)
  ;; (funcall draw-rect x y r g b a)
  )

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
	 (script-ids (map-scripts-to-run map))
	 (scripts (fset:image (lambda (id)
				(get-prop-in engine-doc (list "SCRIPTS" id)))
			      script-ids)))
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
  (with-slots* (layers) map
    (layer-width (car layers))))

(defun-export! map-height (map)
  (with-slots* (layers) map
    (layer-height (car layers))))

(defun get-tile-at (layer x y)
  (with-slots* (qmapper.layer:tiles) layer
    (get-in qmapper.layer:tiles (list x y))))
  
(defun fetch-tile-from-tileset (tileset x y)
  (when tileset
    (let ((tiles (tileset-tiles tileset)))
      (get-in tiles (list x y)))))

;; (defmethod draw ((map qmap) &key renderer dst (x 0) (y 0))
;;   ;;(declare (optimize debug))
;;   (if-let (*local-document* (if (equalp dst :ENGINE)
;; 				*engine-document*
;; 				qmapper.root:*document*))
;;     (let* (;;(map (get-prop (root-maps root) (root-chosenmap root)))
;; 	   (hitdata (map-hit-layer map))
;; 	   (w (map-width map))
;; 	   (h (map-height map))
;; 	   (x-coords (mapcar #'dec (range w)))
;; 	   (y-coords (mapcar #'dec (range h)))
;; 	   (layers (->> (map-layers map)
;; 			(remove-if-not #'layer-visible))) ;;old layer-list
;; 	   ;;(layers (length layer-list))
;; 	   ;;(l-coords (reverse (mapcar #'dec (range layers))))
;; 	   (sprites (map-sprites map))
;; 	   ;;(root-sprites (root-sprites *local-document*))
;; 	   (animations (map-animatedsprites map))
;; 	   ;;(root-animations (root-animatedsprites *local-document*))
;; 	   ;; (final-l-coords (->> l-coords
;; 	   ;; 			(remove-if-not (lambda (l-index)
;; 	   ;; 					 (let ((layer (get-layer root map l-index)))
;; 	   ;; 					   (layer-visible layer))))))
;; 	   )
      
;;       (->> layers
;; 	   (mapcar-with-index (lambda (layer index)
;; 				;;(let ((layer (get-prop (root-layers root) (get-prop layer-list l))))
;; 				(let ((coord-pairs (pairs x-coords y-coords)))
;; 				  (dolist (pair coord-pairs)
;; 				    ;;(format t "Drawing tile at ~a~%" pair)
;; 				    (let ((x (car pair))
;; 					  (y (cadr pair)))
				      
;; 				      (if-let (tile (get-tile-at layer x y))
;; 					(progn (let* ((rotation (tile-rotation tile))
;; 						      (sprite (tile-sprite tile))
;; 						      (tile (if sprite
;; 								tile
;; 								(when-let (tile (fetch-tile-from-tileset (nth (tile-tileset tile) (root-tilesets *document*))
;; 													 (tile-x tile)
;; 													 (tile-y tile)))
;; 								  tile)))
;; 						      ;; (subtile (if (not (zerop index))
;; 						      ;; 		    (let ((subtile (get-tile-at map (nth (dec index) final-l-coords) x y)))
;; 						      ;; 		      (if (valid-gl-key? (tile-gl-key subtile))
;; 						      ;; 			  subtile
;; 						      ;; 			  (fetch-tile-from-tileset root
;; 						      ;; 						   (tile-tileset subtile)
;; 						      ;; 						   (tile-x subtile)
;; 						      ;; 						   (tile-y subtile))))))
;; 						      )
						 
;; 						 (when tile
;; 						   (draw tile
;; 							 :rotation rotation
;; 							 :renderer renderer
;; 							 :x (* x 50)
;; 							 :y (* y 50)
;; 							 :opacity (layer-opacity layer))))))))))))
;;       (if (hit-tool-chosen?)
;; 	  (let ((pairs (pairs x-coords y-coords)))
;; 	    (dolist (pair pairs)
;; 	      (let* ((x (car pair))
;; 		     (y (cadr pair))
;; 		     (hit-tile (get-prop-in hitdata (list x y))))
		
;; 		(draw-colored-rect (* x 50)
;; 				   (* y 50)
;; 				   (if hit-tile
;; 				       0
;; 				       255)
;; 				   (if hit-tile
;; 				       255 0)
;; 				   0
;; 				   127)))))

;;       (dolist (sprite sprites)
;;       	(draw sprite :renderer renderer))
      	

;;       (dolist (animation animations)
;; 	(draw animation :renderer renderer)))))

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
