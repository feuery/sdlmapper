(defpackage :qmapper.layer
  ;; (:import-from :fset :size)
  (:use :common-lisp
        :cl-arrows
	:multimethods
	:qmapper.export
	:qmapper.std
	:qmapper.tile)
  (:export :layer-id :layer-name :layer-tiles :tiles :layer-width :layer-height :layer :layer-visible :layer-opacity))

(in-package :qmapper.layer)

(defclass* layer
    (name "")
  (id (random 99999))
  (opacity 255)
  (visible t)
  (tiles '()))

(defparameter +layer-tiles+ nil)

(defun layer-width (*this*)
  (length (Layer-tiles *this*)))
(defun layer-height (*this*)

  (setf +layer-tiles+ (first (Layer-tiles *this*)))
  (let ((result 
	 (length (first (Layer-tiles *this*)))))

    result))

(defun-export! make-2d (w h tile-value)
  (let ((all-tiles
	 (repeatedly (lambda (x)
  		       (repeatedly (lambda (y)
				     (if (or (eq tile-value nil)
					     (eq tile-value t))
					 tile-value
					 (clone tile-value))) h)) w)))
    all-tiles))

(defun-export! make-tiles (w h)
  (make-2d w h (make-tile :x 0 :y 0 :tileset 0 :rotation 0)))

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
						       (fset:with-first tiles (make-tile :x 0 :y 0 :tileset 0 :rotation 0)))))
	    (mapcar #'dec (range width)) :initial-value layer)))

;;      ;; horizontal-growth: cons to end a list of tiles with the length of (map-height map)
;;      ;; vertical-growth: cons a tile to end of all the lists-of-tiles 
(defun-export! grow-layer-horizontally (layer)
  (update-prop layer "TILES" (lambda (tiles)
			       (let ((list-len (fset:size (fset:first tiles))))
				 (fset:with-last tiles (list-of (make-tile :x 0 :y 0 :tileset  0 :rotation 0) list-len))))))

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
