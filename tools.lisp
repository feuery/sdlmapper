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
  (with-slots* (qmapper.root:maps qmapper.root:chosenmap qmapper.root:chosenlayer) root 
    (let* ((map (nth chosenmap maps))
	   ;; my god this with-slots* hodge-podge could use some clojurification in the form of update-in
	   (map (with-slots* (layers) map
		  (let ((layer (nth chosenlayer layers))
			(local-layers layers))
		    (setf (nth chosenlayer local-layers) (with-slots* (tiles) layer
						     (let ((tile (get-in tiles (list tile-x tile-y))))
						       (setf tiles (set-in tiles (list tile-x tile-y)
									   (with-slots* (rotation) tile
									     (setf rotation (mod (inc rotation) 4))))))))
		    (setf layers local-layers)))))
      (setf (nth chosenmap maps) map))))

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


	       
