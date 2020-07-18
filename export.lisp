(defpackage :qmapper.export
  (:use :common-lisp
   :cl-arrows)
  (:import-from :fset :seq :empty-map :convert :with :lookup))

(in-package :qmapper.export)

;; change to t if you need do-log to trace everything

(defmacro defmacro-export! (name &rest rst)
  `(progn
					;(format t "Current package is ~a~%" *package*)
     (defmacro ,name ,@rst)
     (export (quote ,name))))

(export 'defmacro-export!)

(defmacro-export! defun-export! (name &rest rst)
  `(progn
     (defun ,name ,(car rst)
       (do-log (format nil "~aing " ,(symbol-name name))
	 ,@(cdr rst)))
     (export (quote ,name))))

(defmacro-export! defvar-export!
    (name value)
  `(progn
     (defvar ,name ,value)
     (export (quote ,name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar-export! log-everything? nil))

(defmacro-export! do-log (message &rest body &environment env)
  (if log-everything?
      `(progn
  	 (format t "~a~%" ,message)
  	 (let ((result (progn ,@body)))
  	   (format t "not ~a~%" ,message)
  	   result))
      `(progn ,@body)))

(defvar-export! get-current-doc nil)
(defvar-export! explode nil)
(export 'explode)

(defvar-export! lambdas (empty-map))

(defvar-export! load-image nil)
(defvar-export! image-w nil)
(defvar-export! image-h nil)
(defvar-export! copy-image nil)

(defvar-export! add-to-drawingqueue nil)
(defvar-export! clear-drawingqueue nil)
(defvar-export! set-img-x nil)
(defvar-export! set-img-y nil)
(defvar-export! set-img-opacity nil)

(defvar-export! do-schedule-lambda nil)

;;(defvar-export! render nil)
(defvar-export! add-lambda-to-drawingqueue nil)
(defvar-export! clear-lisp-drawingqueue nil)
(defvar-export! set-img-rotation nil)
(defvar-export! MsTime nil)
(defvar-export! image-file-dimensions nil)
(defvar-export! set-img-subobj nil)

(defvar-export! keyDown nil)

(defvar-export! draw-rect nil)
(defvar-export! cpp-hittool-chosen nil)

(defun-export! render-img (dst img)
  nil ;; (funcall render dst img)
  )

(defvar-export! pop_kbd nil)
(defun-export! pop-kbd ()
  nil)
