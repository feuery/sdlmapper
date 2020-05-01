(defpackage :qmapper.tools
  (:use :cl
	:cl-arrows		  
	:qmapper.tile
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
  (with-slots (qmapper.root:maps qmapper.root:chosenmap) root
    (let ((map (nth qmapper.root:chosenmap qmapper.root:maps)))
      (set-tile-at map (root-chosenlayer root) tile-x tile-y selected-tile))))

(deftool :rotation (root x y tile-x tile-y selected-tile)
  (handler-case 
      (let* ((map (root-get-chosen-map root))
	     (last-layer (car (last (map-layers map))))
	     (really-selected-tile (-> (layer-tiles last-layer)
				       (get-in (list tile-x tile-y)))))
	(with-slots (rotation) really-selected-tile
	  (setf rotation (mod (inc rotation) 4))))
    (error (c)
      (format t "ERROR: ~a~%" c))))
