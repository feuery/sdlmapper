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
  (:export :qmap :map-id :map-name))

(in-package :qmapper.map)

(defun-export! find-nearest  (x y lst)
  (->> lst
       (mapcar (lambda (sprite)
		 (list (distance (sprite-x sprite)
				 (sprite-y sprite)
				 x y)
		       sprite)))
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

(defclass qmap ()
  ((name :accessor map-name :initform (format nil "Map ~a" (incf *map-init-counter*)))
   (layers :accessor map-layers :initform '())
   (id :initform (random 99999) :accessor map-id)
   (sprites :accessor map-sprites :initform '())
   (animatedSprites :accessor map-animatedSprites :initform '())
   (hit-layer :accessor map-hit-layer :initform '())
   (scripts-to-run :accessor map-scripts-to-run :initform '())
   (has-gravity? :accessor map-has-gravity? :initform nil))) ;;validator  #'boolp))

(defmethod initialize-instance :after ((map qmap) &key layer-w layer-h layer-count)
  (with-slots (layers hit-layer) map
    (setf layers (->> (range layer-count)
		      (mapcar #'dec)
		      (mapcar (lambda (index)
				(make-instance 'layer :name (format nil "Layer ~a" index)
					       :tiles (make-tiles layer-w layer-h))))))
    (setf hit-layer (make-hitlayer layer-w layer-h))))



(defun map-findNearest (x y)
  ;; Let's search the nearest animatedsprite or sprite
  (let* ((lst  (->> (map-sprites *this*)
		    (convert 'list)
		    (concatenate 'list 
				 (convert 'list (map-animatedSprites *this*)))
		    (mapcar (lambda (sprite-id)
			      (or 
			       (get-prop (root-sprites *document*) sprite-id)
			       (get-prop (root-animatedSprites *document*) sprite-id)))))))
    (car (find-nearest x y lst))))

      

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


(defun deg->rad (deg)
  (/ (* pi deg) 180))

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

(defmethod draw ((map qmap) &key renderer dst (x 0) (y 0))
  (if-let (*local-document* (if (equalp dst :ENGINE)
				*engine-document*
				qmapper.root:*document*))
    (let* ((root *local-document*)
	   (map (get-prop (root-maps root) (root-chosenmap root)))
	   (hitdata (map-hit-layer map))
	   (w (true-map-width map))
	   (h (true-map-height map))
	   (x-coords (mapcar #'dec (range w)))
	   (y-coords (mapcar #'dec (range h)))
	   (layer-list (map-layers map))
	   (layers (size layer-list))
	   (l-coords (reverse (mapcar #'dec (range layers))))
	   (sprites (map-sprites map))
	   (root-sprites (root-sprites *local-document*))
	   (animations (map-animatedsprites map))
	   (root-animations (root-animatedsprites *local-document*))
	   (final-l-coords (->> l-coords
				(remove-if-not (lambda (l-index)
						 (let ((layer (get-layer root map l-index)))
						   (layer-visible layer)))))))
      
      (->> final-l-coords
	   (mapcar-with-index (lambda (l index)
				(let ((layer (get-prop (root-layers root) (get-prop layer-list l))))
				  (mapcar (lambda (x)
					    (mapcar (lambda (y)
						      (if-let (tile (get-tile-at map l x y))
							(progn (let* ((sprite (tile-sprite tile))
								      (tile (if sprite
										tile
										(when-let (tile (fetch-tile-from-tileset root
															 (tile-tileset tile)
															 (tile-x tile)
															 (tile-y tile)))
										  (assert (string= (q-type-of (make-instance 'qmapper.tile:tile)) "TILE"))
										  tile)))
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
								   (let ((rotation (tile-rotation tile))
									 (sprite (or sprite (tile-sprite tile))))
								     (if sprite
									 (progn
									   (set-image-x :MAP sprite (* 50 x))
									   (set-image-y :MAP sprite (* 50 y))
									   (set-image-opacity :MAP sprite (get-prop layer "opacity"))
									   (set-image-rotation :MAP sprite (deg->rad rotation))


									   ;; (when subtile
									   ;;   (set-image-subobject :MAP tile subtile))
									   
									   (render-img :MAP sprite))))))
							       (if (and (hit-tool-chosen?)
									(string= "MAP VIEW" renderer-name))
								   
								   (let* ((hit-tile (get-prop-in hitdata (list x y)))
									  (hit-tile (if (equalp hit-tile (fset:seq))
											nil
											hit-tile)))
								     
								     (draw-colored-rect (* x 50)
											(* y 50)
											(if hit-tile
											    0
											    255)
											(if hit-tile
											    255 0)
											0
											127))))))
						    y-coords))
					  x-coords)))))

      (dolist (sprite-id (convert 'list sprites))
	(let ((sprite (get-prop root-sprites sprite-id)))
	  (sprite-render sprite)))

      (dolist (animation-id (convert 'list animations))
	(let ((anim (-> (get-prop root-animations animation-id)
			animatedsprite-advanceframeifneeded!)))
	  ;; a surprising skip of the set-doc, to prevent DoSsing dom tree element
	  (if (equalp dst :ENGINE)
	      (setf *engine-document* (set-prop-in *local-document* (list 'animatedSprites (get-prop anim "ID")) anim))
	      (setf qmapper.root:*document* (set-prop-in *local-document* (list 'animatedSprites (get-prop anim "ID")) anim)))
	  (animatedsprite-render anim))))))

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
