;;;; clinch-test.lisp

(in-package #:cl-opengl-test)


(defun move (obj)
  (with-slots (position) obj
    (incf (car position) 1)
    (incf (cadr position) 1)))

(defun render (sprite renderer)
  (qmapper.obj:draw sprite :renderer renderer))

(defparameter *fps* 0)
(defparameter *fps-reset* (sdl2:get-ticks))

(defmulti render-scene #'equalp (renderer root)
  (list app-state editor-state))

(defmultimethod render-scene (list :editor :tileset) (renderer root)
  (let* ((chosen-tileset (root-chosentileset root))
	 (tileset (nth chosen-tileset (root-tilesets root))))
    (if tileset
    	(draw tileset :renderer renderer :x 50 :y 50))))

(defun idle (renderer draw-queue)
  (sdl2:render-clear renderer)
  
  ;; (dolist (sprite draw-queue)
  ;;   (render sprite renderer))

  (render-scene renderer *document*)
  
  (sdl2:render-present renderer)
  (sleep 0.002)

  (incf *fps*)

  (when (> (- (sdl2:get-ticks) *fps-reset*) 1000)
;;    (format t "FPS: ~a~%" *fps*)
    (setf *fps* 0)
    (setf *fps-reset* (sdl2:get-ticks))))

(defun event-loop (renderer)
  ;;(push (qmapper.obj:create-sprite :texture-path "/home/feuer/Sync/qt-test/kaunis_tileset.jpeg" :renderer renderer) *draw-queue*)
  ;; (dotimes (i 20)
  ;;   (push (qmapper.obj:create-sprite :texture-path "/home/feuer/Sync/qt-test/kaunis_tileset.jpeg" :renderer *renderer*) *draw-queue*))
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
	      (let ((scancode (sdl2:scancode-value keysym))
		    (sym (sdl2:sym-value keysym))
		    (mod-value (sdl2:mod-value keysym)))
		(cond
		  ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
		  ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
		  ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))
		(format t "Key sym: ~a, code: ~a, mod: ~a~%"
			sym
			scancode
			mod-value)))

    (:keyup (:keysym keysym)
	    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	      (sdl2:push-event :quit)))
    (:idle ()
	   (idle renderer *draw-queue*))

    (:quit () t)))

(defparameter *renderer* nil)

(defun main ()
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
					;    (format t "Opengl version ~a~%" (gl:get* :version))
    (sdl2:with-window (win :title "qmapper without the q" :flags '(:shown :resizable))
      (let* ((renderer (sdl2:create-renderer win)))
	(sdl2:set-render-draw-color renderer 255 0 0 255)
	(setf *renderer* renderer)
	(event-loop renderer)))))

;; (main)

;; (with-slots (tilesets) *document*
;;   (push
;;    (make-instance 'qmapper.tileset:tileset :tileset-path "/home/feuer/Sync/qt-test/kaunis_tileset.jpeg" :renderer *renderer*)
;;    tilesets))

;; (dolist (sprite *draw-queue*)
;;   (with-slots (qmapper.obj:position) sprite
;;     (setf qmapper.obj:position (list (random 800) (random 600)))))

;; (let ((first-tileset (nth (random (length *draw-queue*)) *draw-queue*)))
;;   (if (not (equalp (sdl2:surface-width (qmapper.obj:sprite-surface first-tileset)) 50))						 
;;       (push (qmapper.obj:create-subsprite first-tileset (list 300 50 50 50) *renderer*) *draw-queue*)))

;; (dolist (sprite *draw-queue*)
;;   (with-slots (qmapper.obj:opacity) sprite
;;     (setf qmapper.obj:opacity (random 255))))


;; (dolist (sprite *draw-queue*)
;;   (with-slots (qmapper.obj:angle) sprite
;;     (setf qmapper.obj:angle (random 360))))
