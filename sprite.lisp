(defpackage :qmapper.sprite
  (:use :common-lisp
	:cl-strings
	:qmapper.export
        :cl-arrows
	:multimethods
	:qmapper.std
	:qmapper.tileset
	:qmapper.root
	:qmapper.obj)
  (:export :update-map-sprite :init-sprite :set-angle :qsprite :sprite-x :sprite-y :set-pos :get-pos :sprite-angle))

(in-package :qmapper.sprite)

(defun-export! validate-x (x sprite)
  (let ((map (get-prop-in *document* (list "MAPS" (root-chosenmap *document*))))
	(sprite-w (funcall image-w (get-prop sprite "GL-KEY"))))
    (< (+ sprite-w x) (* (true-map-width map) 50))))

(defun-export! validate-y (y sprite)
  (let ((map (get-prop-in *document* (list "MAPS" (root-chosenmap *document*))))
	(sprite-h (funcall image-h (get-prop sprite "GL-KEY"))))
    (< (+ sprite-h y) (* (true-map-height map) 50))))


(defmulti set-pos #'equalp (sprite-lookalike x y)
  (fset:lookup sprite-lookalike "TYPE"))

(defmulti get-pos #'equalp (sprite-lookalike)
  (fset:lookup sprite-lookalike "TYPE"))

(defmulti set-angle #'equalp (sprite-lookalike angle)
  (fset:lookup sprite-lookalike "TYPE"))

;; this could probs be replaced with qmapper.obj:sprite?
(defclass* sprite
  (x 0) ;; validator #'validate-x)
   (y 0) ;; validator #'validate-y)
   (angle 0) ;; validator 0.0)
  (id (random 99999))
   (gravity-vector #[0 1] ;; (lambda (g)
		   ;;   (and (fset:seq? g)
		   ;; 	    (every #'numberp (fset:convert 'list g))))
		   )
   (visible  t)
   ;;(parentMapId :initarg :parentMapId :accessor sprite-parentMapId :initform "")
   (name  "")
   ;;(loadingDone :initarg :loadingDone :accessor sprite-loadingDone :initform nil)
   (obj-sprite :not-loaded-correctly))

(defmultimethod set-pos "sprite" (sprite new-x new-y &key)
  (with-slots* (x y) sprite
    (setf x new-x
	  y new-y)))

(defmultimethod set-angle "sprite" (sprite new-angle &key)
  (with-slots* (angle) sprite
    (setf angle new-angle)))
		  
(defmultimethod get-pos "sprite" (sprite &key)
  (with-slots* (x y) sprite :read-only
    (list x y)))

(defun init-sprite (sprite &key sprite-path renderer)
  (if (and sprite-path
	   renderer)
      (with-slots* (obj-sprite) sprite
	(setf obj-sprite (create-sprite :renderer renderer :texture-path sprite-path)))
      sprite))

(defmultimethod draw "sprite" (sprite args)
  (let ((renderer (fset:lookup args "RENDERER")))
    (with-slots* (x y angle obj-sprite) sprite
      (setf obj-sprite
	    (let ((obj-sprite2 obj-sprite))
	      (with-slots* (obj-angle position) obj-sprite2
		(setf position (list x y)
		      obj-angle angle)
		(assert (hash-table-p obj-sprite2))
		(draw obj-sprite2 (fset:map ("RENDERER" renderer)))))))))

(defmulti update-map-sprite #'equalp (map new-sprite)
  (fset:lookup new-sprite "TYPE"))

(defmultimethod update-map-sprite "sprite" (map new-sprite)
  (with-slots* (sprites) map
    (setf sprites (mapcar (lambda (a-sprite)
			    (let ((id (fset:lookup a-sprite "ID")))
			      (if (equalp id (fset:lookup new-sprite "ID"))
				  new-sprite
				  a-sprite)))
			  sprites))))

