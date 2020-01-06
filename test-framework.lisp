(defpackage :qmapper.test-framework
  (:use :common-lisp
   :qmapper.root
   :qmapper.std
   :cl-arrows)

  (:import-from :qmapper.export :defmacro-export! :defun-export! :defvar-export!)  
  (:import-from :fset :size :convert :empty-map :with))

(in-package :qmapper.test-framework)



(defvar *tests* (empty-map))

(defmacro-export! deftest (name &rest forms)
  `(progn
     (setf *tests* (with *tests* (format nil "~a" (quote ,name)) (lambda () ,@forms)))))

(defmacro-export! is (a b)
  `(progn
     (let ((result (equalp ,a ,b)))
       (if result
	   :pass
	   (progn
	     (format t "(not= ~a ~a)~%" ,a ,b)
	     :fail)))))

(defmacro-export! assert-some? (val)
  (let ((val-str (prin1-to-string val)))
    `(progn
       (format t "asserting ~a~%" ,val-str)
       (assert ,val)
       ,val)))

(defun-export! run-tests ()
  (let ((failed 0)
	(qmapper.root:*document* (init-root!)))
    (fset:do-map (name test *tests*)
      (format t "running ~a~%" name)
      (let ((res (handler-case (funcall test)
		   (error (c)
		     (format t "~a threw error ~a~%" name c)
		     :fail))))
      	(if (equalp res :pass)
      	    (format t "~a passed~%" name)
      	    (incf failed))))
    (if (pos? failed)
	(format t "~a failed~%" failed))))
