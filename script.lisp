(defpackage :qmapper.script
  (:use :common-lisp
	:cl-arrows
	:qmapper.std
	:qmapper.export
	;; :qmapper.root
	))

(in-package :qmapper.script)

(defclass* script 
    (contents "")
  (name "")
  (ns "user"))
		 
