(defpackage :qmapper.sprite
  (:use :common-lisp
	:cl-strings
	:qmapper.export
	:cl-arrows
	:qmapper.std
	:qmapper.tileset
	:qmapper.root
	:qmapper.obj)
  (:export :set-angle :qsprite :sprite-x :sprite-y :set-pos :get-pos :sprite-angle))

(in-package :qmapper.sprite)

(defun-export! validate-x (x sprite)
  (let ((map (get-prop-in *document* (list "MAPS" (root-chosenmap *document*))))
	(sprite-w (funcall image-w (get-prop sprite "GL-KEY"))))
    (< (+ sprite-w x) (* (true-map-width map) 50))))

(defun-export! validate-y (y sprite)
  (let ((map (get-prop-in *document* (list "MAPS" (root-chosenmap *document*))))
	(sprite-h (funcall image-h (get-prop sprite "GL-KEY"))))
    (< (+ sprite-h y) (* (true-map-height map) 50))))

(defgeneric set-pos (sprite-lookalike x y &key))
(defgeneric get-pos (sprite-lookalike &key))
(defgeneric set-angle (sprite-lookalike angle &key))

;; this could probs be replaced with qmapper.obj:sprite?
(defclass qsprite ()
  ((x :initarg :x :initform 0 :accessor sprite-x) ;; validator #'validate-x)
   (y :initarg :y :initform 0 :accessor sprite-y) ;; validator #'validate-y)
   (angle :initarg :angle :initform 0 :accessor sprite-angle) ;; validator 0.0)
   
   (gravity-vector :initarg :gravity-vector :accessor sprite-gravity-vector :initform #[0 1] ;; (lambda (g)
		   ;;   (and (fset:seq? g)
		   ;; 	    (every #'numberp (fset:convert 'list g))))
		   )
   (visible :initarg :visible :accessor sprite-visible :initform t)
   ;;(parentMapId :initarg :parentMapId :accessor sprite-parentMapId :initform "")
   (name :initarg :name :accessor sprite-name :initform "")
   ;;(loadingDone :initarg :loadingDone :accessor sprite-loadingDone :initform nil)
   (obj-sprite :initarg :obj-sprite :accessor sprite-obj-sprite :initform :not-fucking-loaded-correctly)))

(defmethod set-pos ((sprite qsprite) new-x new-y &key)
  (with-slots (x y) sprite
    (setf x new-x
	  y new-y)))
(defmethod set-angle ((sprite qsprite) angle &key)
  (setf (sprite-angle sprite) angle))
		  
(defmethod get-pos ((sprite qsprite) &key)
  (with-slots (x y) sprite
    (list x y)))

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
