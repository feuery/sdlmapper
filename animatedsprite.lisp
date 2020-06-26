(defpackage :qmapper.animatedsprite
  (:use :common-lisp
        :cl-arrows
	:multimethods 
	:qmapper.obj
	:qmapper.sprite
	:qmapper.tileset
        :qmapper.std
	:qmapper.export
        :qmapper.root)
  (:export animatedsprite))

(in-package :qmapper.animatedsprite)

(defclass* animatedsprite
    (name "")
  (currentFrameId 0 )
  (msPerFrame 25)
  (animationPlaying t)
  (visible t)
  (lastUpdated (get-ms-time))
  
  (x 0)
  (y 0)
  (angle 0.0)
  ;; noteditable tag should prevent this field appearing in propeditor
  (sprites '()))


(defun animatedsprite-maxFrames (*this*)
  (length (animatedsprite-sprites *this*)))

(defmultimethod draw 'animatedsprite (animation args)
  ;; TODO this messing around with sprite index does not in fact belong in the drawing function
  (setf *document*
	(with-slots* (qmapper.root:maps qmapper.root:chosenmap qmapper.root:chosenlayer) *document*
	  (let ((the-maps maps))
	    (setf (nth chosenmap the-maps)
		  (let* ((map (nth chosenmap maps))
			 (renderer (fset:lookup args "RENDERER"))
			 (animation-index (position animation (fset:lookup map "ANIMATEDSPRITES") :test #'equalp)))
		    (with-slots* (animatedsprites) map
		      (setf (nth animation-index animatedsprites)
			    (with-slots* (sprites x y currentframeid angle ) (-> animation
										 animatedsprite-advanceframeifneeded!)
				(setf (nth currentframeid sprites)
				      (with-slots* (position) (nth currentframeid sprites)
					(setf position (list x y))
					(setf (gethash "ANGLE" (nth currentframeid sprites))  angle)
					(draw (nth currentframeid sprites) (fset:map ("RENDERER" renderer))))))))))
	    (setf qmapper.root:maps the-maps)))))

(defmultimethod set-pos 'animatedsprite (animation new-x new-y)
  (with-slots* (x y) animation
    (setf x new-x
	  y new-y)))

(defmultimethod get-pos 'animatedsprite (animation)
   (with-slots* (x y) animation
     (list x y)))

(defmultimethod set-angle 'animatedsprite (animation new-angle)
  (with-slots* (angle)
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


