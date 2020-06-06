(defpackage :qmapper.engine_events
  (:use :common-lisp
	:cl-arrows
	:qmapper.export 
	:qmapper.std)
  (:shadowing-import-from :fset :empty-map :with-first :with :seq :image :lookup ;; :filter
			  :reduce :size :concat :convert :wb-map-from-list))

(in-package :qmapper.engine_events)

(defvar *engine-key-map* (empty-map))

(defun-export! add-engine-key-fn (key fn)
  (setf *engine-key-map* (with *engine-key-map* key fn)))

(defun-export! get-engine-lambda (key)
  (lookup *engine-key-map* key))
