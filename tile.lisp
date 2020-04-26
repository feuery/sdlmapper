(defpackage :qmapper.tile
  (:use :common-lisp
	:cl-arrows
	:qmapper.obj
	:qmapper.std)
  (:export :tile :tile-x :x :y :tile-y :tileset :tile-tileset :rotation :tile-rotation :sprite :tile-sprite))

(in-package :qmapper.tile)

(defclass tile ()
    ((x :initarg :x :accessor tile-x :initform 0)
     (y :initarg :y :accessor tile-y :initform 0)
     (tileset :initarg :tileset :accessor tile-tileset :initform 0) 
     (rotation :initarg :rotation :accessor tile-rotation :initform 0)
     (sprite :initarg :sprite :accessor tile-sprite :initform nil)))

(defmethod draw ((tile-obj tile) &key renderer x y (opacity 0))
  (let ((tile-op opacity))
  (with-slots (sprite) tile-obj
    (with-slots (position opacity) sprite
      (setf position (list x y))
      (setf opacity tile-op)
      (draw sprite :renderer renderer)))))

;; (export-all :qmapper.tile)
