(defpackage :qmapper.script
  (:use :common-lisp
	:cl-arrows
	:qmapper.std
	:qmapper.export
	;; :qmapper.root
	))

(in-package :qmapper.script)

(defcppclass Script
    (public
     (properties
      (contents "")
      (name "")
      (ns "user")
      (script_type "lisp"))
     (functions
      (is-lisp? ()
		(let ((script-type (Script-script_type *this*)))
		  ;; (format t "script type: ~a, is it lisp? ~a~%" script-type (equalp script-type 'lisp))
		  (string= (format nil "~a" script-type) "lisp")))
      (is-glsl? ()
		(let ((script-type (Script-script_type *this*)))
		  (equalp script-type 'glsl))))))

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
		 
