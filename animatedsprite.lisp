(defpackage :qmapper.animatedsprite
  (:use :common-lisp
        :cl-arrows
	:qmapper.sprite
	:qmapper.tileset
        :qmapper.std
	:qmapper.export
	:qmapper.root))

(in-package :qmapper.animatedsprite)

(defclass animatedsprite ()
  ((parentMapId :initarg :parentMapId :accessor animatedsprite-parentMapId :initform "" )
   (name :initarg :name :accessor animatedsprite-name :initform "")
   (currentFrameId :initarg :currentFrameId :accessor animatedsprite-currentFrameId :initform 0 )
   (msPerFrame :initarg :msPerFrame :accessor animatedsprite-msPerFrame :initform 25)
   (animationPlaying :initarg :animationPlaying :accessor animatedsprite-animationPlaying :initform t)
   (lastUpdated :initarg :lastUpdated :accessor animatedsprite-lastUpdated :initform 0 )
   
   (x :initarg :x :accessor animatedsprite-x :initform 0)
   (y :initarg :y :accessor animatedsprite-y :initform 0)
   (angle :initarg :angle :accessor animatedsprite-angle :initform 0.0)
   ;; noteditable tag should prevent this field appearing in propeditor
   (sprites :initarg :sprites :accessor animatedsprite-sprites :initform '())))

(defun animatedsprite-maxFrames (*this* )
  (size (animatedsprite-sprites *this*)))

(defun animatedsprite-advanceFrame! (*this* )
  (let* ((tt *this*)
	 (frameid (inc (animatedsprite-currentFrameId tt)))
	 (frameid (mod frameid (animatedsprite-maxFrames tt))))
    (set-animatedsprite-currentFrameId! tt frameid)))

(defun animatedsprite-render (*this* )
  (let* ((sprites (animatedsprite-sprites *this*))
	 (current-sprite (-> (get-prop sprites (animatedsprite-currentFrameId *this*))
			     (set-sprite-x! (animatedsprite-x *this*))
			     (set-sprite-y! (animatedsprite-y *this*))
			     (set-sprite-angle! (animatedsprite-angle *this*)))))
    (sprite-render current-sprite)))

(defun animatedsprite-advanceFrameIfNeeded! (*this* )
  (if (and (animatedsprite-animationPlaying *this*)
	   (> (get-ms-time) (+ (animatedsprite-lastUpdated *this*)
			       (animatedsprite-msPerFrame *this*))))
      (-> (animatedsprite-advanceFrame! *this*)
	  (set-animatedsprite-lastUpdated! (get-ms-time)))
      *this*))

(defun-export! load-animation (path framecount framelifetime)
  (let* ((dimensions (img-file-dimensions path))
	 (_ (format t "dimensions are ~a~%" dimensions))
	 (width (first dimensions))
	 (height (second dimensions))
	 (root-imgs (load-tilesetless-texture-splitted path :tile-width (floor (/ width framecount)) :tile-height height))
	 (_ (format t "root-imgs: ~a~%" root-imgs))
	 (sprites (->> (range framecount)
		       (mapcar (lambda (i)
				 (load-sprite-rootless (get-prop-in root-imgs (list (dec i) 0))))))))
    (format t "sprites:~a~%" sprites)
    (make-animatedsprite :name "New animatedsprite" :lastUpdated (get-ms-time) :sprites sprites)))

