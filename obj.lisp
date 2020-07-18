(defpackage #:qmapper.obj
  (:use #:cl
	:qmapper.std
	:multimethods
	:qmapper.export
   #:cl-arrows)
  (:export :create-subsprite :angle :obj-size :opacity :obj-surface :position :draw :create-sprite :*draw-queue*))

(in-package #:qmapper.obj)

(defclass* obj
  (position nil)
   ;; in pixels
   (size nil)
   ;; in degrees
  (obj-angle 0)
   ;; the error color used when can't render surface (I guess)
  (color nil)
   ;; 255...0, opaque...translucent
  (opacity 255)
   ;; we have to keep hold of this for subobject purposes
  (surface nil (nonserializable))
   ;; "HW-accelerated" version of the above
  (texture nil (nonserializable))
  (id (random 9999999)))

(defun init-obj (obj &key sdl-surface renderer)
  (with-slots* (position size obj-angle color opacity surface texture) obj
    (setf surface sdl-surface
	  texture (sdl2:create-texture-from-surface renderer sdl-surface)
	  position (list 0 0)
	  size (list (sdl2:surface-width sdl-surface)
		     (sdl2:surface-height sdl-surface))
	  obj-angle 0
	  color (list 0 0 255)
	  opacity 255)
    (sdl2:set-texture-blend-mode texture :blend)))

(defmulti draw #'equalp (drawable args)
  (fset:lookup drawable "TYPE"))

(defmultimethod draw "obj" (obj args)
  (let ((renderer (fset:lookup args "RENDERER")))
    (with-slots* (position texture size opacity obj-angle) obj
      (sdl2:with-rects ((dst-rect (car position) (cadr position)
				  (car size) (cadr size)))
	(sdl2:set-texture-alpha-mod texture  opacity)
	(sdl2:render-copy-ex renderer texture :dest-rect dst-rect
					      :angle obj-angle)))))

(defun xor (&rest args)
  (flet ((xor2 (a b) (not (eq (not a) (not b)))))
    (cond ((null args)  t)
          ((null (cdr args)) (car args))
          (t (apply (function xor) (xor2 (car args) (cadr args)) (cddr args))))))

(defun create-sprite (&key texture-path renderer surface)
  (assert (xor texture-path surface))
  (cond (texture-path (-> (make-obj) (init-obj :sdl-surface (sdl2-image:load-image texture-path) :renderer renderer)))
	(surface (-> (make-obj) (init-obj :sdl-surface surface :renderer renderer)))))

(defun create-subsprite (src-sprite rect renderer)
  "Copies a rect from src-sprite. Rect is a list with params: (x y w h)"
  (let (;;(size (obj-size src-sprite))
	(surface (obj-surface src-sprite)))
    (sdl2:with-rects ((real-rect (first rect) (second rect) (third rect) (fourth rect)))
      (let ((subsurface (sdl2:create-rgb-surface (third rect) (fourth rect) 32)))
	(sdl2:blit-surface surface real-rect subsurface nil)

	(create-sprite :renderer renderer :surface subsurface)))))


(defparameter *draw-queue* (list))

(defun-export! clear-draw-queue ()
  (setf *draw-queue* nil))

(defun-export! add-to-drawqueue (img)
  (push img *draw-queue*))
