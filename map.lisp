(defpackage :qmapper.map
  (:use :common-lisp
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
  (:import-from :qmapper.export :clear-lisp-drawingqueue :add-lambda-to-drawingqueue)
  (:import-from :fset :size :convert))

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

(defun-export! invert-hit-tile (map x y)
  ;; get-prop-in is a bit stupid and coerces nils to #[ ], of which #'not doesn't understand anything
  (update-prop-in map (list "HIT-LAYER" x y) (lambda (hit-obj)
					       (if (equalp hit-obj (fset:seq))
						   t
						   (not hit-obj)))))

(defun-export! invert-hit-tile-in-chosen-map (root x y)
  (update-prop-in root (list "MAPS" (root-chosenmap root))
		  (lambda (map)
		    (invert-hit-tile map x y))))

(defun boolp (b &rest rst)
  (typep b 'boolean))

(defcppclass Map
    (public 
     (properties
      (name "Map 1")
      (layers '())
      (sprites '())
      (animatedSprites '())
      (hit-layer '())
      (scripts-to-run '())
      (has-gravity? nil #'boolp))
     (functions
      (findNearest (x y)
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
      (height ()
	      (format t "Calling deprecated map-height~%")
	      (qmapper.root:true-map-height *this*))
      (width ()
	     (format t "Calling deprecated map-width~%")
	     (qmapper.root:true-map-width *this*))
      ;; this'll be fun
      (resize (w
	       h
	       vAnchor
	       hAnchor)
	      ;; TODO IMPLEMENT
	      (error "Don't call Map-resize yet!")))))



(defun-export! make-map-with-layers (doc name w h layer-count)
  (let* ((layers (repeatedly (lambda (i)
			       (let ((l 
				       (make-Layer :name (str (prin1-to-string i) "th layer")
						   :tiles
						   (make-tiles w h))))
					;(format t "making layer ~a ~%" l)
				 l)) layer-count))
	 (ids (mapcar (lambda (l) (get-prop l "ID")) layers))
	 (map (make-map :name name
			:layers ids
			:sprites '()
			:animatedSprites '()
			:hit-layer (make-hitlayer w h)
			:scripts-to-run '())))
    (-> (set-root-layers! doc
			  (reduce (lambda (all-layers layer)
				    (set-prop all-layers (get-prop layer "ID") layer)) layers :initial-value (root-layers doc)))
	(set-root-maps! (set-prop (root-maps doc) (get-prop map "ID") map))
	(set-root-chosenmap! (get-prop map "ID")))))

(defun-export! get-tile-at2 (layer x y)
  (get-prop-in (root-layers *document*) (list layer 'tiles x y)))

(defun-export! get-tile-at (map layer x y)
  (let ((l (get-prop (root-layers *document*) (get-prop (map-layers map) layer))))
    (get-prop-in l (list "tiles" x y))))

(defun-export! find-layer-parent (layer root)
  (let* ((maps (mapcar #'cdr (convert 'list (root-maps root)))))
    (car (remove-if-not (lambda (map)
			  (let ((layers (convert 'list (map-layers map))))
			    (position layer layers :test #'string=)))
			maps))))

(defun-export! find-sprite-parent (sprite root)
  (let* ((maps (mapcar #'cdr (convert 'list (root-maps root)))))
    (car (remove-if-not (lambda (map)
			  (let ((sprites (convert 'list (map-sprites map))))
			    (position sprite sprites :test #'string=)))
			maps))))

(defun-export! find-animatedsprite-parent (animatedsprite root)
  (let* ((maps (mapcar #'cdr (convert 'list (root-maps root)))))
    (car (remove-if-not (lambda (map)
			  (let ((animatedsprites (convert 'list (map-animatedsprites map))))
			    (position animatedsprite animatedsprites :test #'string=)))
			maps))))

(defun-export! drop-layer (map layer-id)
  (update-prop map "LAYERS" (lambda (layer-list)
			      (fset:filter (lambda (l-id)
					     (not (equalp l-id layer-id)))
					   layer-list))))

(defun-export! drop-map-layer (root map-id layer-id)
  (let ((root (update-prop-in root (list "MAPS" map-id) (lambda (map)
							  (drop-layer map layer-id)))))
    (if (equalp (root-chosenLayerInd root) layer-id)
	(let ((layer-id (-> root
			    root-maps
			    (get-prop (root-chosenmap root))
			    map-layers
			    (get-prop 0))))
	  (format t "fixin the chosenlayer to ~a~%" layer-id)
	  (set-root-chosenlayerind! root layer-id))
	root)))







(defun-export! drop-animatedsprite (map animatedsprite-id)
  (update-prop map "ANIMATEDSPRITES" (lambda (animatedsprite-list)
			      (fset:filter (lambda (l-id)
					     (not (equalp l-id animatedsprite-id)))
					   animatedsprite-list))))

(defun-export! drop-map-animatedsprite (root map-id animatedsprite-id)
  (update-prop-in root (list "MAPS" map-id) (lambda (map)
					      (drop-animatedsprite map animatedsprite-id))))

(defun-export! drop-sprite (map sprite-id)
  (update-prop map "SPRITES" (lambda (sprite-list)
			      (fset:filter (lambda (s-id)
					     (not (equalp s-id sprite-id)))
					   sprite-list))))

(defun-export! drop-map-sprite (root map-id sprite-id)
  (update-prop-in root (list "MAPS" map-id) (lambda (map)
					      (drop-sprite map sprite-id))))
	 
(defun-export! index-of-chosen-map (root)
  (root-chosenMap root))

(defun-export! get-tile-at-chosen-map (root x y)
  (let ((m (root-chosenMap root))
	(l (root-chosenLayerInd root)))
    (get-tile-at m l x y)))

(defun set-tile-innest (tiles x y tile)
  (set-prop-in tiles (list x y) tile))
    
(defun-export! set-tile-inner (root map layer x y tile)
  (let* ((layer-id layer)
	 (layers (map-layers map))
	 (layer (get-prop (root-layers root) layer-id))
	 (layer (set-layer-tiles! layer (set-tile-innest (layer-tiles layer) x y tile))))
    (set-root-layers! root (set-prop (root-layers root) layer-id layer))))

(defun-export! set-tile-at (root x y tile)
  (let* ((ind (root-chosenMap root))
	 (maps (root-maps root))
	 (map (get-prop maps ind)))
    (set-tile-inner root
		    map
		    (root-chosenLayerInd root)
		    x
		    y
		    tile)))

(defun-export! set-chosen-tile-at (root x y)
  (assert (root-chosenTile root))
  (set-tile-at root x y (root-chosenTile root)))

(defun-export! set-tile-at-chosen-map (root x y tile)
  (set-tile-at root x y tile))

(defun fetch-tile-from-tileset (root tileset x y)
  (let ((tileset (->> root
		      (root-tilesets)
		      (convert 'list)
		      (mapcar #'cdr)
		      (nth tileset))))
    ;; (format t "(< ~a ~a)~%(< ~a ~a)~%"
    ;; 	    x (tileset-w tileset)
    ;; 	    y (tileset-h tileset))

    (when tileset
      
      (assert (< x (tileset-w tileset)))
      (assert (< y (tileset-h tileset)))
      (get-prop-in tileset (list "tiles" x y)))))

(defun-export! set-tile-rotation-at (root x y rotation)
  (let* ((layer (get-prop (root-layers root) (root-chosenLayerInd root)))
	 (tile (-> layer
		   (get-prop-in (list 'tiles x y))
		   (set-prop 'rotation rotation)))
	 (layer (set-prop-in layer (list 'tiles x y) tile)))
    (set-root-layers! root
		      (set-prop (root-layers root) (root-chosenLayerInd root) layer))))

(defun-export! add-layer (root map-index)
  (let ((l nil))
    (-> root
	(update-prop-in (list "MAPS" map-index) (lambda (m)
						 (let* ((layers (Map-layers m))
							(w (true-map-width m))
							(h (true-map-height m))
							(ll (make-Layer :name (str (prin1-to-string (size layers)) "th layer")
									:tiles
												      (make-tiles w h))))
						   ;; ei
						   (setf l ll)
						   (update-prop m "LAYERS" (lambda (layers)
									     (fset:insert layers 0 (get-prop l "ID")))))))
	(update-prop "LAYERS" (lambda (layers)
				(set-prop layers (get-prop l "ID") l))))))

(defun-export! add-layer-to-selected-map (root)
  (add-layer root (root-chosenMap root)))

(defun-export! delete-layer (root map-index layer-index)
  (set-root-maps! root
		  (-> (root-maps root)
		      (alist-update map-index (lambda (m)
						(set-Map-layers! m (drop-list-i (Map-layers m) layer-index)))))))

;; I hope putting those lambdas here too will keep them from being collected
(defvar *lisp-dq-buffer* (list))

(defun-export! clear-lisp-dq (dst)
  (setf *lisp-dq-buffer* (list))
  (funcall clear-lisp-drawingqueue (symbol-name dst)))

(defun-export! add-to-lisp-qd (dst fn)
  ;; doesn't work on sbcl
  ;; (ext:set-finalizer fn (lambda (x)
  ;; 			  (format t "Finalizing lisp-dq-element ~a~%" x)))
  (setf *lisp-dq-buffer* (cons fn *lisp-dq-buffer*))
  (funcall add-lambda-to-drawingqueue dst fn))

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

(defun set-image-subobject (dst tile subtile)
  (funcall set-img-subobj dst (tile-gl-key tile) (tile-gl-key subtile)))

(defun draw-colored-rect (x y r g b a)
  (funcall draw-rect x y r g b a))

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

(defun-export! set-map-renderer-fn (dst map-id)
  (add-to-lisp-qd dst (lambda (renderer-name)
			;; (format t "renderer name is ~a~%" renderer-name)
			(setf map-id (root-chosenmap *document*))
			;; (setf root-maps (root-maps *document*))
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
											  (progn (let ((tile (if (valid-gl-key? (tile-gl-key tile))
														 tile
														 (fetch-tile-from-tileset root
																	  (tile-tileset tile)
																	  (tile-x tile)
																	  (tile-y tile))))
												       (subtile (if (not (zerop index))
														    (let ((subtile (get-tile-at map (nth (dec index) final-l-coords) x y)))
														      (if (valid-gl-key? (tile-gl-key subtile))
															  subtile
															  (fetch-tile-from-tileset root
																		   (tile-tileset subtile)
																		   (tile-x subtile)
																		   (tile-y subtile)))))))
												   
												   (when tile
												     (let ((rotation (tile-rotation tile))
													   (gl-key (tile-gl-key tile)))
												       (if (valid-gl-key? gl-key)
													   (progn
													     (set-image-x :MAP tile (* 50 x))
													     (set-image-y :MAP tile (* 50 y))
													     (set-image-opacity :MAP tile (get-prop layer "opacity"))
													     (set-image-rotation :MAP tile (deg->rad rotation))


													     (when subtile
													       (set-image-subobject :MAP tile subtile))
													     
													     (render-img :MAP gl-key))))))
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
				      (animatedsprite-render anim))))))))

(defun-export! select-map-layer (root map-id layer-id)
  (clear-lisp-dq :MAP)
  (set-map-renderer-fn :MAP map-id)
  
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
