(defpackage :qmapper.tile
  (:use :common-lisp
	:cl-arrows
	:qmapper.std))

(in-package :qmapper.tile)

(defcppclass Tile 
  (public
   (properties
    (x 0)
    (y 0)
    (tileset 0) 
    (rotation 0)
    (gl-key nil))))

;; (export-all :qmapper.tile)
