(defpackage :qmapper.tile
  (:use :common-lisp
	:cl-arrows
	:multimethods
	:qmapper.obj
	:qmapper.std)
  (:export :tile :tile-x :x :y :tile-y :tileset :tile-tileset :rotation :tile-rotation :sprite :tile-sprite))

(in-package :qmapper.tile)

(defclass* tile
    (x 0)
  (y 0)
  (tileset 0) 
  (rotation 0)
  (sprite nil))

(defmultimethod draw 'tile (tile-obj &key renderer x y rotation (opacity 255))
  (let ((tile-op opacity))
    (with-slots* (sprite) tile-obj
      (let ((rotation (or rotation
			  (tile-rotation tile-obj))))
	(with-slots* (angle position opacity) sprite
	  (setf angle (* 90 rotation))
	  (setf position (list x y))
	  (setf opacity tile-op)
	  (draw sprite :renderer renderer))))))

;; (export-all :qmapper.tile)
