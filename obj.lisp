(defpackage #:qmapper.obj
  (:use #:cl
   :qmapper.std
   #:cl-arrows)
  (:export :create-subsprite :angle :opacity :sprite-surface :position :draw :create-sprite))

(in-package #:qmapper.obj)

(defclass sprite ()
  ((position :accessor sprite-position)
   ;; in pixels
   (size :accessor sprite-size)
   ;; in degrees
   (angle :accessor sprite-angle)
   ;; the error color used when can't render surface (I guess)
   (color :accessor sprite-color)
   ;; 255...0, opaque...translucent
   (opacity :accessor sprite-opacity)
   ;; we have to keep hold of this for subobject purposes
   (surface :accessor sprite-surface)
   ;; "HW-accelerated" version of the above
   (texture :accessor sprite-texture)))

(defmethod initialize-instance :after ((obj sprite) &key sdl-surface renderer)
  (with-slots (position size angle color opacity surface texture) obj
    (setf surface sdl-surface
	  texture (sdl2:create-texture-from-surface renderer sdl-surface)
	  position (list 0 0)
	  size (list (sdl2:surface-width sdl-surface)
		     (sdl2:surface-height sdl-surface))
	  angle 0
	  color (list 0 0 255)
	  opacity 255)
    (sdl2:set-texture-blend-mode texture :blend)))

(defgeneric draw (drawable &key))
(defmethod draw ((obj sprite) &key renderer)
  (with-slots (position texture size opacity angle) obj
    (sdl2:with-rects ((dst-rect (car position) (cadr position)
				(car size) (cadr size)))
      (sdl2:set-texture-alpha-mod texture  opacity)
      (sdl2:render-copy-ex renderer texture :dest-rect dst-rect
      			   :angle angle))))

(defun xor (&rest args)
  (flet ((xor2 (a b) (not (eq (not a) (not b)))))
    (cond ((null args)  t)
          ((null (cdr args)) (car args))
          (t (apply (function xor) (xor2 (car args) (cadr args)) (cddr args))))))

(defun create-sprite (&key texture-path renderer surface)
  (assert (xor texture-path surface))
  (cond (texture-path (make-instance 'sprite :sdl-surface (sdl2-image:load-image texture-path) :renderer renderer))
	(surface (make-instance 'sprite :sdl-surface surface :renderer renderer))))

(defun create-subsprite (src-sprite rect renderer)
  "Copies a rect from src-sprite. Rect is a list with params: (x y w h)"
  (with-slots (size surface) src-sprite
    (sdl2:with-rects ((real-rect (first rect) (second rect) (third rect) (fourth rect)))
      (let ((subsurface (sdl2:create-rgb-surface (third rect) (fourth rect) 32)))
	(sdl2:blit-surface surface real-rect subsurface nil)

	(create-sprite :renderer renderer :surface subsurface)))))
