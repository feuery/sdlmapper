(defpackage :qmapper.gravity-loop
  (:use :common-lisp
	:qmapper.std
	:qmapper.engine_events
	:qmapper.app-state 
	:qmapper.root
	:qmapper.map
	:qmapper.sprite
	:qmapper.export
	:cl-arrows
	:bordeaux-threads))

(in-package :qmapper.gravity-loop)

(defvar-export! *gravity-loop-running?* t)

;;(setf *gravity-loop-running?* nil)

(defun gravity-loop ()
  (format t "Starting gravity loop~%")
  (setf *gravity-loop-running?* t)
  (while *gravity-loop-running?*
    (when (equalp app-state :engine)
      (setf *engine-document* 
	    (with-slots* (chosenmap maps) *engine-document*
	      (let ((map (-> (nth chosenmap maps)
			     (update-prop "SPRITES" (partial #'mapcar (lambda (sprite)
								       (if (sprite-gravity-enabled sprite)
									   (with-slots* (gravity-vector x y) sprite
									     (setf x (+ x (first gravity-vector))
										   y (+ y (second gravity-vector))))
									   sprite)))))))
		(setf (nth chosenmap maps) map)))))
    (sleep 0.5))
  (format t "Stopping gravity loop~%"))

(defun-export! start-gravity-loop! ()
  (make-thread #'gravity-loop :name "Gravity thread"))
	  
(start-gravity-loop!)
