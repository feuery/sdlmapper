(defpackage :qmapper.engine
  (:use :common-lisp
	:cl-arrows
	:qmapper.export
	:qmapper.sprite
	:qmapper.root 
	:qmapper.std
	:bordeaux-threads)
  (:shadowing-import-from :fset :first)
  (:import-from :fset :do-seq :image :less-first ))

(in-package :qmapper.engine)

(defun fset-second (seq)
  (if seq
      (first (less-first seq))))

;; gravity

(defvar-export! *gravity-loop-running?* nil)
(defvar *gravity-thread* nil)

(defun gravity-loop ()
  (while *gravity-loop-running?*
    (let* ((sprite-ids (get-prop-in *engine-document* (list "MAPS" (root-chosenmap *engine-document*) "SPRITES")))
	   (sprites (fset:image (lambda (id)
				  (get-prop-in *engine-document* (list "SPRITES" id)))
				sprite-ids)))
      (do-seq (sprite sprites)
	(let* ((gravity-vec (or (sprite-gravity-vector sprite) #[0 0]))
	       (x (first gravity-vec))
	       (y (fset-second gravity-vec))
	       (id (get-prop sprite "ID")))
	  ;; (format t "Gravity-looping x, y: ~a~%" (list x y))
	  (if (or (not (zerop x))
		  (not (zerop y)))
	      (set-engine-doc
	       (let ((*silence-validators* t))
		 ;; (format t "*silence-validators*: ~a~%" *silence-validators*)
		 (update-prop-in *engine-document* (list "SPRITES" id) (lambda (sprite)
									 (-> sprite
									     (update-prop "X" (partial #'+ x))
									     (update-prop "Y" (partial #'+ y)))))))))))
    (ms-sleep 2)))

(defun-export! start-gravity-loop! ()
  (setf *gravity-loop-running?* t)
  (setf *gravity-thread* (make-thread #'gravity-loop :name "Gravity loop")))

(defun-export! stop-gravity-loop! ()
  (setf *gravity-loop-running?* nil)
  (if *gravity-thread*
      (join-thread *gravity-thread*))
  (setf *gravity-thread* nil))


;; (stop-gravity-loop!)
;; (start-gravity-loop!)
