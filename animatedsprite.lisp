(defpackage :qmapper.animatedsprite
  (:use :common-lisp
        :cl-arrows
	:multimethods 
	:qmapper.obj
	:qmapper.app-state
	:qmapper.sprite
	:qmapper.tileset
        :qmapper.std
	:qmapper.export
        :qmapper.root)
  (:export animatedsprite :animatedsprite-gravity-enabled :animatedsprite-gravity-vector))

(in-package :qmapper.animatedsprite)

(defclass* animatedsprite
    (name "")
  (currentFrameId 0 )
  (id (random 99999))
  (msPerFrame 25)
  (animationPlaying t)
  (visible t)
  (lastUpdated (get-ms-time))
  
  (x 0)
  (y 0)
  (angle 0.0)
  (gravity-vector #[0 1] ;; (lambda (g)
		    ;;   (and (fset:seq? g)
		    ;; 	    (every #'numberp (fset:convert 'list g))))
		  )
  (gravity-enabled nil)

  ;; noteditable tag should prevent this field appearing in propeditor
  (sprites '()))


(defun animatedsprite-maxFrames (*this*)
  (length (animatedsprite-sprites *this*)))

(defun set-sprite-angle (sprite an)
  (with-slots* (angle) sprite
    (setf angle an)))

(defmultimethod draw "animatedsprite" (animation args)
  (if (equalp app-state :editor)
      (setf *document*
	    (with-slots* (qmapper.root:maps qmapper.root:chosenmap qmapper.root:chosenlayer) *document*
	      (let ((the-maps maps))
	      	(setf (nth chosenmap the-maps)
	      	      (let* ((map (nth chosenmap maps))
	      		     (renderer (fset:lookup args "RENDERER"))				
	      		     (animation-index (position animation (clean-hashmaps (fset:lookup map "ANIMATEDSPRITES")) :test #'equalp)))
	      		(if animation-index
	      		  (with-slots* (animatedsprites) map
	      		    (setf (nth animation-index animatedsprites)
	      			  (with-slots* (sprites x y currentframeid angle ) (-> animation
	      									       animatedsprite-advanceframeifneeded!)
	      			    (setf (nth currentframeid sprites)
	      				  (with-slots* (position) (set-sprite-angle (nth currentframeid sprites) angle)
	      				    (setf position (list x y))
	      				    (draw (nth currentframeid sprites) (fset:map ("RENDERER" renderer))))))))
			  map)))
		(assert (not (null (first the-maps))))
	      	(setf qmapper.root:maps the-maps))))
      (setf *engine-document*
	    (with-slots* (maps chosenmap chosenlayer) *engine-document*	      
	      (let ((the-maps maps))
	      	(setf (nth chosenmap the-maps)
	      	      (let* ((map (nth chosenmap maps))
	      		     (renderer (fset:lookup args "RENDERER"))
	      		     (animation-index (position animation (clean-hashmaps (fset:lookup map "ANIMATEDSPRITES")) :test #'equalp)))
			(if animation-index
			    (with-slots* (animatedsprites) map
			      (setf (nth animation-index animatedsprites)
				    (with-slots* (sprites x y currentframeid angle ) (-> animation
											 animatedsprite-advanceframeifneeded!)
				      (setf (nth currentframeid sprites)
					    (with-slots* (position) (set-sprite-angle (nth currentframeid sprites) angle)
					      (setf position (list x y))
					      (draw (nth currentframeid sprites) (fset:map ("RENDERER" renderer))))))))
			    map)))
		(assert (not (null (first the-maps))))
	      	(setf qmapper.root:maps the-maps))))))

(defmultimethod set-pos "animatedsprite" (animation new-x new-y)
  (with-slots* (x y) animation
    (setf x new-x
	  y new-y)))

(defmultimethod get-pos "animatedsprite" (animation)
   (with-slots* (x y) animation :read-only
     (list x y)))

(defmultimethod set-angle "animatedsprite" (animation new-angle)
  (with-slots* (angle) animation
    (setf angle new-angle)))

(defun animatedsprite-advanceFrameIfNeeded! (animation)
  (with-slots* (animationPlaying sprites currentframeid lastUpdated msPerFrame) animation
    (let ((maxframes (length sprites)))
      (when (and animationPlaying		     
		 (> (get-ms-time) (+ lastUpdated
				      msPerFrame)))


	(setf currentframeid (mod (inc currentframeid) maxframes))
	(setf lastUpdated (get-ms-time))))))
      

(defun-export! load-tilesetless-texture-splitted (base-surface surface-dimensions &key renderer (tile-width 50) (tile-height 50))
  (assert renderer)
  (let* ((root-size surface-dimensions)
	 (base-obj (create-sprite :surface base-surface :renderer renderer))

  	 (w (floor (/ (first root-size) tile-width)))
  	 (h (floor (/ (second root-size) tile-height)))

	 (textures (mapcar (lambda (x)
  			     (mapcar (lambda (y)  
  				       (create-subsprite base-obj (list
								   (* x tile-width) (* y tile-height)
								   tile-width tile-height)
							 renderer))
  				     (mapcar #'dec (range h))))
  			   (mapcar #'dec (range w)))))
    (format t "textures ~a loaded!~%" textures)
     textures))

(defun-export! init-animated-sprite (animation &key path framecount renderer)
  (assert (and path framecount renderer))
  (let* (
	 (base-surface (sdl2-image:load-image path))
	 (dimensions (list (sdl2:surface-width base-surface) (sdl2:surface-height base-surface)))
	 ;;	 (_ (format t "dimensions are ~a~%" dimensions))
	 (width (first dimensions))
	 (height (second dimensions))
	 (root-imgs (load-tilesetless-texture-splitted base-surface dimensions :renderer renderer :tile-width (floor (/ width framecount)) :tile-height height))
	 ;;(_ (format t "root-imgs: ~a~%" root-imgs))
	 (loaded-sprites (->> (range framecount)
			      (mapcar #'dec)
			      (mapcar (lambda (i)
					(car (nth i root-imgs)))))))
    (format t "loaded sprites: ~a~%" loaded-sprites)
    (with-slots* (sprites) animation
      (setf sprites loaded-sprites))))



(defmultimethod qmapper.sprite:update-map-sprite "animatedsprite" (map new-sprite)
  (with-slots* (animatedSprites) map
    (setf animatedsprites (mapcar (lambda (a-sprite)
				    (let ((id (fset:lookup a-sprite "ID")))
				      (if (equalp id (fset:lookup new-sprite "ID"))
					  new-sprite
					  a-sprite)))
				  animatedsprites))))
