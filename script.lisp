(defpackage :qmapper.script
  (:use :common-lisp
	:cl-arrows
	:qmapper.std
	:qmapper.export
	;; :qmapper.root
	))

(in-package :qmapper.script)

(defclass script ()
  ((script-contents :initarg :contents :initform "")
   (script-name :initarg :name :initform "")
   (script-ns :initarg :ns :initform "user")
   (script-script_type :initarg :script_type :initform "lisp")))

(defun script-is-lisp? (*this*)
  (let ((script-type (Script-script_type *this*)))
    (string= (format nil "~a" script-type) "lisp")))

(defun script-is-glsl? (*this*)
  (let ((script-type (Script-script_type *this*)))
    (equalp script-type 'glsl)))

(defun-export! drop-script-with-ids (scripts ids)
  (format t "Dropping id's ~a from scripts ~a~%" ids scripts)
  (let* ((scripts-seq? (fset:seq? scripts))
	 (scripts (if (not (listp scripts))
		      (fset:convert 'list scripts)
		      scripts))
	 (ids (if (not (listp ids))
		  (fset:convert 'list ids)
		  ids))
	 (result (remove-if (lambda (script)
			      (member (get-prop script "ID") ids :test #'string=))
			    scripts)))
    (if scripts-seq?
	(fset:convert 'fset:seq result)
	result)))
		 
