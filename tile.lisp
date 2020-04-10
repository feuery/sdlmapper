(defpackage :qmapper.tile
  (:use :common-lisp
	:cl-arrows
	:qmapper.std)
  (:export :tile :tile-x :x :y :tile-y :tileset :tile-tileset :rotation :tile-rotation :sprite :tile-sprite))

(in-package :qmapper.tile)

(defclass tile ()
    ((x :initarg :x :accessor tile-x :initform 0)
     (y :initarg :y :accessor tile-y :initform 0)
     (tileset :initarg :tileset :accessor tile-tileset :initform 0) 
     (rotation :initarg :rotation :accessor tile-rotation :initform 0)
     (sprite :initarg :sprite :accessor tile-sprite :initform nil)))

;; (export-all :qmapper.tile)
