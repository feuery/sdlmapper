(defpackage :qmapper.editor_events
  (:use :common-lisp
	:cl-arrows
	:qmapper.export 
	:qmapper.std)
  (:shadowing-import-from :fset :empty-map :with-first :with :seq :image :lookup ;; :filter
			  :reduce :size :concat :convert :wb-map-from-list))

(in-package :qmapper.editor_events)

(defvar *key-event-map* (empty-map))

(defun-export! add-key-lambda (key lambda)
  (setf *key-event-map* (with *key-event-map* key lambda)))

(defun-export! get-key-lambda (key)
  (lookup *key-event-map* key))
