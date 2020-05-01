(defpackage :qmapper.tools
  (:use :cl
	:qmapper.map
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
