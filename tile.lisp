(defpackage :qmapper.tile
  (:use :common-lisp
	:cl-arrows
	:qmapper.std)
  (:export :tile :tile-x :x :y :tile-y :tileset :tile-tileset :rotation :tile-rotation :sprite :tile-sprite))

(in-package :qmapper.tile)

(defclass tile ()
    ((x :accessor tile-x :initform 0)
     (y :accessor tile-y :initform 0)
     (tileset :accessor tile-tileset :initform 0) 
     (rotation :accessor tile-rotation :initform 0)
     (sprite :accessor tile-sprite :initform nil)))

;; (export-all :qmapper.tile)
