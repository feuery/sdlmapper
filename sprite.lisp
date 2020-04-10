(defpackage :qmapper.sprite
  (:use :common-lisp
	:cl-strings
	:qmapper.export
	:cl-arrows
	:qmapper.std
	:qmapper.tileset
	:qmapper.root
	:qmapper.obj))

(in-package :qmapper.sprite)

(defun-export! validate-x (x sprite)
  (let ((map (get-prop-in *document* (list "MAPS" (root-chosenmap *document*))))
	(sprite-w (funcall image-w (get-prop sprite "GL-KEY"))))
    (< (+ sprite-w x) (* (true-map-width map) 50))))

(defun-export! validate-y (y sprite)
  (let ((map (get-prop-in *document* (list "MAPS" (root-chosenmap *document*))))
	(sprite-h (funcall image-h (get-prop sprite "GL-KEY"))))
    (< (+ sprite-h y) (* (true-map-height map) 50))))


;; this could probs be replaced with qmapper.obj:sprite?
(defclass sprite ()
  ((x :initarg :x :initform 0 :accessor sprite-x) ;; validator #'validate-x)
   (y :initarg :y :initform 0 :accessor sprite-y) ;; validator #'validate-y)
   (angle :initarg :angle :initform 0 :accessor sprite-angle) ;; validator 0.0)
   
   (gravity-vector :initarg :gravity-vector :accessor sprite-gravity-vector :initform #[0 1] ;; (lambda (g)
		   ;;   (and (fset:seq? g)
		   ;; 	    (every #'numberp (fset:convert 'list g))))
		   )
   (parentMapId :initarg :parentMapId :accessor sprite-parentMapId :initform "")
   (name :initarg :name :accessor sprite-name :initform "")
   (loadingDone :initarg :loadingDone :accessor sprite-loadingDone :initform nil)
   (obj-sprite :initarg :obj-sprite :accessor sprite-obj-sprite :initform nil)))


  

(defun-export! load-sprite-rootless (path)
  (cond ((and (stringp path)
	      (probe-file path))
	 (let ((img (load-img path)))
	   (make-sprite :x 0 :y 0 :angle 0.0 :parentMapId nil :name (car (last (split path "/"))) :loadingDone t :obj-sprite img)))
	((or (keywordp path)
	     (stringp path))
	 (let ((img path))
	   (make-sprite :x 0 :y 0 :angle 0.0 :parentMapId nil :name "new sprite" :loadingDone t :obj-sprite img)))
	(t (error "called load-sprite-rootless with invalid data"))))
 
(defun-export! load-sprite (root path)
  (let* ((img  (load-img path))
	 (id   (gensym))
	 (mapInd (root-chosenmap root)))
    (format t "Updating root ~%")
    (push-sprite2 root mapInd id (make-sprite :x 0 :y 0 :angle 0.0 :parentMapId mapInd :name (car (last (split path "/"))) :loadingDone t :obj-sprite img))))

(defun-export! is-sprite? (spr)
  (equalp (q-type-of spr)
	  "Sprite"))
