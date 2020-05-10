(defpackage :qmapper.animatedsprite
  (:use :common-lisp
        :cl-arrows
	:qmapper.obj
	:qmapper.sprite
	:qmapper.tileset
        :qmapper.std
	:qmapper.export
        :qmapper.root)
  (:export animatedsprite))

(in-package :qmapper.animatedsprite)

(defclass animatedsprite ()
  (;; (parentMapId :initarg :parentMapId :accessor animatedsprite-parentMapId :initform "" )
   (name :initarg :name :accessor animatedsprite-name :initform "")
   (currentFrameId :initarg :currentFrameId :accessor animatedsprite-currentFrameId :initform 0 )
   (msPerFrame :initarg :msPerFrame :accessor animatedsprite-msPerFrame :initform 25)
   (animationPlaying :initarg :animationPlaying :accessor animatedsprite-animationPlaying :initform t)
   (visible :initarg :visible :accessor animatedsprite-visible :initform t)
   (lastUpdated :initarg :lastUpdated :accessor animatedsprite-lastUpdated :initform (get-ms-time))
   
   (x :initarg :x :accessor animatedsprite-x :initform 0)
   (y :initarg :y :accessor animatedsprite-y :initform 0)
   (angle :initarg :angle :accessor animatedsprite-angle :initform 0.0)
   ;; noteditable tag should prevent this field appearing in propeditor
   (sprites :initarg :sprites :accessor animatedsprite-sprites :initform '())))

(defun animatedsprite-maxFrames (*this*)
  (length (animatedsprite-sprites *this*)))

(defun animatedsprite-advanceFrame! (animation )
  (let ((maxframes (animatedsprite-maxframes animation)))
    (with-slots (currentframeid) animation
      (setf currentframeid (mod (inc currentframeid) maxframes)))))

(defmethod draw ((animation animatedsprite) &key renderer)
  (with-slots (sprites x y currentframeid angle) animation
      (animatedsprite-advanceframeifneeded! animation)
    (let ((current-sprite (nth currentframeid sprites)))
      (with-slots (position) current-sprite
	(setf position (list x y))
	(setf (obj-sprite-angle current-sprite) angle)
	(draw current-sprite :renderer renderer)))))

(defmethod set-pos ((animation animatedsprite) new-x new-y &key)
  (with-slots (x y) animation
    (setf x new-x
	  y new-y)))
(defmethod get-pos ((sprite animatedsprite) &key)
  (with-slots (x y) sprite
    (list x y)))
(defmethod set-angle ((animation animatedsprite) angle &key)
  (setf (animatedsprite-angle animation) angle))

(defun animatedsprite-advanceFrameIfNeeded! (animation)
  (with-slots (animationPlaying lastUpdated msPerFrame) animation
    (when (and animationPlaying		     
	     (> (get-ms-time) (+ lastUpdated
				 msPerFrame)))
      (animatedsprite-advanceframe! animation)
      (setf lastUpdated (get-ms-time)))))

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

(defmethod initialize-instance :after ((animation animatedsprite) &key path framecount renderer)
  (assert (and path framecount renderer))
  (let* ((base-surface (sdl2-image:load-image path))
	 (dimensions (list (sdl2:surface-width base-surface) (sdl2:surface-height base-surface)))
	 (_ (format t "dimensions are ~a~%" dimensions))
	 (width (first dimensions))
	 (height (second dimensions))
	 (root-imgs (load-tilesetless-texture-splitted base-surface dimensions :renderer renderer :tile-width (floor (/ width framecount)) :tile-height height))
	 (_ (format t "root-imgs: ~a~%" root-imgs))
	 (loaded-sprites (->> (range framecount)
			      (mapcar #'dec)
			      (mapcar (lambda (i)
					(car (nth i root-imgs)))))))
    (format t "loaded sprites: ~a~%" loaded-sprites)
    (with-slots (sprites) animation
	(setf sprites loaded-sprites))))

