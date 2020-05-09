(defpackage :qmapper.sprite
  (:use :common-lisp
	:cl-strings
	:qmapper.export
	:cl-arrows
	:qmapper.std
	:qmapper.tileset
	:qmapper.root
	:qmapper.obj)
  (:export :qsprite))

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
(defclass qsprite ()
  ((x :initarg :x :initform 0 :accessor sprite-x) ;; validator #'validate-x)
   (y :initarg :y :initform 0 :accessor sprite-y) ;; validator #'validate-y)
   (angle :initarg :angle :initform 0 :accessor sprite-angle) ;; validator 0.0)
   
   (gravity-vector :initarg :gravity-vector :accessor sprite-gravity-vector :initform #[0 1] ;; (lambda (g)
		   ;;   (and (fset:seq? g)
		   ;; 	    (every #'numberp (fset:convert 'list g))))
		   )
   ;;(parentMapId :initarg :parentMapId :accessor sprite-parentMapId :initform "")
   (name :initarg :name :accessor sprite-name :initform "")
   ;;(loadingDone :initarg :loadingDone :accessor sprite-loadingDone :initform nil)
   (obj-sprite :initarg :obj-sprite :accessor sprite-obj-sprite :initform nil)))


(defmethod initialize-instance :after ((sprite qsprite) &key sprite-path renderer)
  (if (and sprite-path
	   renderer)
      (with-slots (obj-sprite) sprite
	(setf obj-sprite (create-sprite :renderer renderer :texture-path sprite-path))))
  sprite)

(defmethod draw ((sprite qsprite) &key renderer)
  (with-slots (x y angle obj-sprite) sprite
    (let ((sp-angle angle))
      (with-slots (position qmapper.obj:angle) obj-sprite
	(setf position (list x y)
	      qmapper.obj:angle sp-angle)
	(draw obj-sprite :renderer renderer)))))
