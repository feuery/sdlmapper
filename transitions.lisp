(defpackage :qmapper.transitions
  (:use :common-lisp
	:qmapper.std
	:ppcre
	:qmapper.root 
	:cl-arrows
	:qmapper.export
	:bordeaux-threads))

(in-package :qmapper.transitions)

(defun-export! make-event (state-change-fn doc-path rest-params)
  (fset:map ("STATE-CHANGE-FN" state-change-fn)
	    ("DOC-PATH" doc-path)
	    ("REST-PARAMS" rest-params)))

(defvar-export! *message-queue* nil)

(defvar *dispatcher-running* t)
(defparameter *effects* (fset:empty-map))

(defmacro-export! defeffect (event-name param-list &rest lambda)
  `(setf *effects* (set-prop *effects* ,event-name (make-event 
						    (lambda ,param-list
						      ,@lambda)
						    nil
						    nil))))

(defeffect "PRINT-HELLOWORLD" (doc data-from-previous-event)
  (format t "here's the REST-PARAMETERS you received from the previous effect ~a~%" data-from-previous-event)
  (format t "hello from effect, here's some sprites: ~a~% " (root-sprites doc))
  ;; you're supposed to return at least the doc, and if requiring more effects, those
  (fset:map ("DOC" doc)))
		     

(defun handle-event (doc event)
  (let* ((fn (get-prop event "state-change-fn"))
	 (doc-path (get-prop event "doc-path"))
	 (rest-params (get-prop event "rest-params"))
	 (obj (if doc-path
		  (get-prop-in doc doc-path)
		  doc))
	 ;; (_ (format t "funcalling fn , rest params ~a~%" rest-params))
	 (new-obj (apply fn obj rest-params nil))
	 ;; (_ (format t "funcalled fn ~%"))
	 ;; new value is found under "DOC", rest is metadada
	 (updated-doc-value (get-prop new-obj "DOC"))
	 (new-doc (if doc-path
		      (set-prop-in doc doc-path updated-doc-value)
		      updated-doc-value))
	 (effects (->> (get-prop new-obj "EFFECTS")
		       (mapcar (partial #'get-prop *effects*))
		       (mapcar (lambda (effect)
				 (set-prop effect "REST-PARAMS" (list (-> new-obj
									  (dissoc-prop "DOC")
									  (dissoc-prop "EFFECTS"))))))))
	 (meta-keys (fset:filter (lambda (a)
				   (not (string= a "DOC")))
				 (fset-map-keys new-obj))))
    (if effects
	(reduce #'handle-event
		effects :initial-value new-doc)
	new-doc)))
    
(defun dispatcher ()
  (setf *dispatcher-running* t)
  (while *dispatcher-running*
    (if-let (event (pop *message-queue*))
      (set-engine-doc (handle-event *engine-document* event)))
    (ms-sleep 2)))

(defun-export! start-dispatcher! ()
  (make-thread #'dispatcher :name "Dispatcher thread"))

;; (setf *dispatcher-running* nil)
(start-dispatcher!)
;; this adds the values in the REST-PARAMETERS field to x of the first sprite
;; (push (make-event (lambda (x &rest rest-params)
;; 		    (fset:map ("DOC" (apply #'- x (car rest-params)))
;; 			      ("EFFECTS" (list "PRINT-HELLOWORLD"))
;; 			      ("DADA" (list 1 2 3))))
;; 		  (list "SPRITES" (caar (fset:convert 'list (root-sprites *engine-document*))) "X")
;; 		  (list 20 30 100 200))
;;       *message-queue*)
