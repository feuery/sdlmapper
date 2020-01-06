(defpackage :qmapper.layer
  (:use :common-lisp
        :cl-arrows
	:qmapper.export
	:qmapper.std
	:qmapper.tile)
  (:import-from :fset :size))

(in-package :qmapper.layer)

(defcppclass Layer
  (public
   (properties
    (name "")
    (opacity 255)
    (visible t)
    (tiles '())
    ;; (parent nil)
    )
   (functions
    (width ()
	   (size (Layer-tiles *this*)))
    (height ()
	    (size (fset-first (Layer-tiles *this*)))))))

(defun-export! make-2d (w h tile-value)
  (let ((all-tiles
	 (repeatedly (lambda (x)
  		       (repeatedly (lambda (y)
  				     tile-value) h)) w)))
    all-tiles))

(defun-export! make-tiles (w h)
  (make-2d w h (make-Tile :x 0 :y 0 :tileset 0 :rotation 0
			  :gl-key nil)))

(defun-export! make-hitlayer (w h)
  (make-2d w h t))

(defun-export! shrink-layer-vertically (layer)
  (let ((width (layer-width layer)))
    (reduce (lambda (layer i)
	      (update-prop-in layer (list "TILES" i) #'fset:less-first))
	    (mapcar #'dec (range width)) :initial-value layer)))

(defun-export! grow-layer-vertically (layer)
  (let ((width (layer-width layer)))
    (reduce (lambda (layer i)
	      (update-prop-in layer (list "TILES" i) (lambda (tiles)
						       (fset:with-first tiles (make-tile :x 0 :y 0 :tileset 0 :rotation 0
											 :gl-key nil)))))
	    (mapcar #'dec (range width)) :initial-value layer)))

;;      ;; horizontal-growth: cons to end a list of tiles with the length of (map-height map)
;;      ;; vertical-growth: cons a tile to end of all the lists-of-tiles 
(defun-export! grow-layer-horizontally (layer)
  (update-prop layer "TILES" (lambda (tiles)
			       (let ((list-len (fset:size (fset:first tiles))))
				 (fset:with-last tiles (list-of (make-tile :x 0 :y 0 :tileset  0 :rotation 0 :gl-key  nil) list-len))))))

(defun-export! shrink-layer-horizontally (layer)
  (update-prop layer "TILES" #'fset:less-last))

(defun-export! resize-layer (layer new-w new-h)
  (let ((w-diff (- new-w (layer-width layer)))
	(h-diff (- new-h (layer-height layer))))
    (cond ((pos? w-diff) (resize-layer (grow-layer-horizontally layer) new-w new-h))
	  ((pos? h-diff) (resize-layer (grow-layer-vertically layer) new-w new-h))
	  ((neg? w-diff) (resize-layer (shrink-layer-horizontally layer) new-w new-h))
	  ((neg? h-diff) (resize-layer (shrink-layer-vertically layer) new-w new-h))
	  (t layer))))
