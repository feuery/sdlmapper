;; ; -*- mode: lisp; -*-

;; (defpackage :qmapper.testi-map1
;;   (:use :common-lisp
;;         :cl-arrows
;; 	:qmapper.engine_events
;; 	:qmapper.layer
;; 	:rutils.abbr
;;         :qmapper.std
;; 	:qmapper.root
;; 	:qmapper.map
;; 	:qmapper.sprite
;; 	:qmapper.export
;; 	:qmapper.script)
;;   (:shadowing-import-from :cl-strings :replace-all))

;; (in-package :qmapper.testi-map1)

;; (defparameter sprite (with-slots* (maps chosenmap) *engine-document* :read-only
;; 			  (let ((map (nth chosenmap maps)))
;; 			    (first (map-sprites map)))))

;; (qmapper.engine_events:add-engine-key-fn "Up"
;; 					 (lambda ()
;; 					   (setf *engine-document* (walk-and-transform (lambda (v)
;; 											 (and (or (fset:map? v) (hash-table-p v))
;; 											      (equalp (fset:lookup v "TYPE") "SPRITE")
;; 											      (equalp (fset:lookup v "ID") (fset:lookup sprite "ID"))))
;; 										       (lambda (sprite)
;; 											 (with-slots* (x y) sprite
;; 											   (setf x (+ 10 x)
;; 												 y (+ 10 y))))
;; 										       *engine-document*))))
