(defpackage :qmapper.animatedsprite
  (:use :common-lisp
        :cl-arrows
	:qmapper.sprite
	:qmapper.tileset
        :qmapper.std
	:qmapper.export
	:qmapper.root)
  (:import-from :fset :size))

(in-package :qmapper.animatedsprite)

(defun get-ms-time ()
  (funcall MsTime))

(defcppclass animatedsprite
    (public
     (properties
      (parentMapId "" )
      (name "")
      (currentFrameId 0 )
      (msPerFrame 25)
      (animationPlaying t)
      (lastUpdated 0 )
      
      (x 0)
      (y 0)
      (angle 0.0)
      ;; noteditable tag should prevent this field appearing in propeditor
      (sprites '() ))
     (functions
      (maxFrames ()
		 (size (animatedsprite-sprites *this*)))
      (advanceFrame! ()
		     (let* ((tt *this*)
			    (frameid (inc (animatedsprite-currentFrameId tt)))
			    (frameid (mod frameid (animatedsprite-maxFrames tt))))
		       (set-animatedsprite-currentFrameId! tt frameid)))
      
      (render ()
    	      (let* ((sprites (animatedsprite-sprites *this*))
    		     (current-sprite (-> (get-prop sprites (animatedsprite-currentFrameId *this*))
					 (set-sprite-x! (animatedsprite-x *this*))
					 (set-sprite-y! (animatedsprite-y *this*))
					 (set-sprite-angle! (animatedsprite-angle *this*)))))
		(sprite-render current-sprite)))
      (advanceFrameIfNeeded! ()
    			     (if (and (animatedsprite-animationPlaying *this*)
    				      (> (get-ms-time) (+ (animatedsprite-lastUpdated *this*)
							  (animatedsprite-msPerFrame *this*))))
				 (-> (animatedsprite-advanceFrame! *this*)
				     (set-animatedsprite-lastUpdated! (get-ms-time)))
				 *this*)))))

(defun img-file-dimensions (path)
  (funcall image-file-dimensions path))

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

