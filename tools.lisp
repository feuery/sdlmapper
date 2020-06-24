(defpackage :qmapper.tools
  (:use :cl
	:cl-arrows		  
	:qmapper.tile
	:qmapper.sprite
	:qmapper.std
	:qmapper.map
	:qmapper.layer
	:qmapper.root)  
  (:shadowing-import-from :fset :map :with :lookup)
  (:export :deftool :*tools*))

(in-package :qmapper.tools)

(defparameter *tools* (map))

(defmacro deftool (name tool-params &rest tool-body)
  `(setf *tools* (with *tools* ,name (lambda ,tool-params
				      ,@tool-body))))

(deftool :pen (root x y tile-x tile-y selected-tile)
  (with-slots* (qmapper.root:maps qmapper.root:chosenmap) root 
    (let ((map (nth qmapper.root:chosenmap qmapper.root:maps)))
      (unless (root-chosenlayer root)
	(format t "chosen layer is nil, stuff will break~%"))
      (let ((result (set-tile-at map (root-chosenlayer root) tile-x tile-y selected-tile)))
	(setf (nth chosenmap maps) result)))))

(deftool :rotation (root x y tile-x tile-y selected-tile)
  (handler-case 
      (let* ((map (root-get-chosen-map root))
	     (last-layer (car (last (map-layers map))))
	     (really-selected-tile (-> (layer-tiles last-layer)
				       (get-in (list tile-x tile-y)))))
	(with-slots* (rotation) really-selected-tile
	  (setf rotation (mod (inc rotation) 4))))
    (error (c)
      (format t "ERROR: ~a~%" c))))

(deftool :sprite-mover (root x y tile-x tile-y selected-tile)
  (let* ((map (root-get-chosen-map root))
	 (nearest (map-findnearest map x y)))
    (set-pos nearest x y)))

(defun angle (x1 y1 x2 y2)
  (let ((a (- x1 x2))
	(b (- (- y1 y2))))
    (atan a b)))

(defun deg->rad (deg)
  (/ (* pi deg) 180))
(defun rad->deg (rad)
  (* (/ rad pi) 180))


(deftool :sprite-rotater (root x y tile-x tile-y selected-tile)
  (let* ((map (root-get-chosen-map root))
	 (nearest (map-findnearest map x y))
	 (position (get-pos nearest)))
    (set-angle nearest (rad->deg (angle 
				  (car position) (cadr position)
				  x y)))))


	       
