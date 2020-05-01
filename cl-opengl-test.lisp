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
	(draw tileset :renderer renderer))))

(defmultimethod render-scene (list :editor :map) (renderer root)
  (let* ((chosen-map (root-chosenmap root)))
    (when (>= chosen-map 0)
      (let ((map (nth chosen-map (root-maps root))))
	(if map
	    (draw map :renderer renderer))))))

(defmulti handle-drag #'equalp (root x y left-or-right)
  (list app-state editor-state))

(defmultimethod handle-drag  (list :editor :map) (root x y left-or-right)
  (if (equalp left-or-right :left)
      (with-slots (chosentool chosenmap chosenlayer chosentile) root
	(let* ((tile-x (floor (/ x 50)))
	       (tile-y (floor (/ y 50)))
	       (tool-is-already-applied-here (get-in dragged-table (list tile-x tile-y))))
	  (unless tool-is-already-applied-here
	    (funcall (fset:lookup qmapper.tools:*tools* chosentool) root x y tile-x tile-y (clone chosentile))
	    (setf (nth tile-y (nth tile-x dragged-table)) t))))))

(defmultimethod handle-drag (list :editor :tileset) (root x y left-or-right)
  (if (equalp left-or-right :left)
      (with-slots (tilesets chosentileset chosentile) root
	(let* ((tile-x (floor (/ x 50)))
	       (tile-y (floor (/ y 50)))
	       (tile (get-in (tileset-tiles (nth chosentileset tilesets)) (list tile-x tile-y))))
	  (setf chosentile tile)))))


(defun idle (renderer draw-queue)
  (sdl2:render-clear renderer)
  
  ;; (dolist (sprite draw-queue)
  ;;   (render sprite renderer))

  (render-scene renderer *document*)
  
  (sdl2:render-present renderer)
  (sleep 0.002)

  (dolist (cmd *sdl-single-command-queue*)
    (funcall cmd))
  (setf *sdl-single-command-queue* nil)

  (incf *fps*)
  (when (> (- (sdl2:get-ticks) *fps-reset*) 1000)
;;    (format t "FPS: ~a~%" *fps*)
    (setf *fps* 0)
    (setf *fps-reset* (sdl2:get-ticks))))

(defvar +left-mouse-button+ 1)
(defvar +right-mouse-button+ 3)

(defun start-drag (root)
  (let* ((map (qmapper.root:root-get-chosen-map root))
	 (w (map-width map))
	 (h (map-height map)))
    (setf dragged-table (make-2d w h nil))))

(defun stop-drag ()
  (setf dragged-table nil))
  

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

    (:mousebuttondown ()
		      (start-drag *document*))
    (:mousebuttonup ()
		    (stop-drag))

    (:mousemotion (:x x :y y)
		  (if (or (sdl2:mouse-state-p +left-mouse-button+)
			  (sdl2:mouse-state-p +right-mouse-button+))
		      (handle-drag (correct-document)
				   x y 
				   (cond ((sdl2:mouse-state-p +left-mouse-button+) :left)
					 ((sdl2:mouse-state-p +right-mouse-button+) :right)
					 (t nil)))))
		      
    (:idle ()
	   (idle renderer *draw-queue*))

    (:quit () t)))

(defun main ()
  (sdl2:with-init (:everything)
    (format t "Using SDL Library Version: ~D.~D.~D~%"
            sdl2-ffi:+sdl-major-version+
            sdl2-ffi:+sdl-minor-version+
            sdl2-ffi:+sdl-patchlevel+)
    (sdl2:with-window (win :title "qmapper without the q" :flags '(:shown :resizable))
      (let* ((renderer (sdl2:create-renderer win)))
	(sdl2:set-render-draw-color renderer 255 0 0 255)
	(setf *renderer* renderer)
	(event-loop renderer)))))

;; (main)

;; (setf *document* (qmapper.root:init-root!))

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
