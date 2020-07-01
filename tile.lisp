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

(defmultimethod draw "tile" (tile-obj args)
  (let* ((renderer (fset:lookup args "RENDERER"))
	 (x (fset:lookup args "X"))
	 (y (fset:lookup args "Y"))
	 (rotation (fset:lookup args "ROTATION"))
	 (opacity (or (fset:lookup args "OPACITY") 255))
	 (tile-op opacity))
    ;; (format t "Drawing tile at ~a~%" (list x y))
    (with-slots* (sprite) tile-obj
		 (setf sprite
		       (let* ((rotation (or rotation
					    (tile-rotation tile-obj)))
			      (new-sprite
			       (with-slots* (angle position opacity) sprite
					    (setf angle (* 90 rotation))
					    (setf position (list x y))
					    (setf opacity tile-op))))
			 (draw new-sprite (fset:map ("RENDERER" renderer))))))))

;; (export-all :qmapper.tile)
