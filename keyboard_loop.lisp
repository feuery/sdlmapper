(defpackage :qmapper.keyboard_loop
  (:use :common-lisp
	:qmapper.std
	:qmapper.engine_events
	:ppcre
	:qmapper.root 
	:cl-arrows
	:qmapper.export
	:bordeaux-threads))

(in-package :qmapper.keyboard_loop)

(defvar-export! *kbd-loop-running?* t)

(defun kbd-loop ()
  (while *kbd-loop-running?*
    (if-let (keys-down (pop-kbd))
	(progn
	  ;; (format t "Received keys: ~a~%" keys-down)
	  (dolist (key-str keys-down)
	    (if-let (engine-lambda (get-engine-lambda key-str))
	      (funcall engine-lambda)))))
    (ms-sleep 2)))

(defun-export! start-kbd-loop! ()
  (make-thread #'kbd-loop :name "Keyboard thread"))
	  
;; (start-kbd-loop!)
