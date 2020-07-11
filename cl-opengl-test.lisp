;;;; clinch-test.lisp

(in-package #:cl-opengl-test)


(defun move (obj)
  (with-slots* (position) obj
    (incf (car position) 1)
    (incf (cadr position) 1)))

(defun render (sprite renderer)
  (qmapper.obj:draw sprite (fset:map ("RENDERER" renderer))))

(defparameter *fps* 0)
(defparameter *fps-reset* (sdl2:get-ticks))

(defmulti render-scene #'equalp (renderer root)
  (list app-state editor-state))

(defmultimethod render-scene (list :editor :tileset) (renderer root)
  (let* ((chosen-tileset (root-chosentileset root))
	 (tileset (nth chosen-tileset (root-tilesets root))))
    (if tileset
	(draw tileset (fset:map ("RENDERER" renderer))))))

(defmultimethod render-scene (list :editor :map) (renderer root)
  (let* ((chosen-map (root-chosenmap root)))
    ;; Tää ajetaan sen jälkeen kun kynää on käytetty
    (if (equalp chosen-map :not-found)
	(format t "Why is chosen-map not found~%"))
    ;(handler-case 
	(when (>= chosen-map 0)
	  (let ((map (nth chosen-map (root-maps root))))
	    (when map
	      (draw map (fset:map ("RENDERER" renderer))))))
      ;; (ERROR (e)
	;; 	(format t "error ~a~%" e)))
    ))

(defmultimethod render-scene (list :engine :tileset) (renderer root)
  (let ((editor-state :map))
    (render-scene renderer root)))

(defmultimethod render-scene (list :engine :map) (renderer root)
  ;; root is always *document*
  ;; thus this architecture is a bit stupid
  (with-slots* (chosenmap maps) *engine-document* :read-only
    (let ((map (nth chosenmap maps)))
      (draw map (fset:map ("RENDERER" renderer))))))

(defmulti handle-drag #'equalp (root x y left-or-right)
  (list app-state editor-state))

(defmultimethod handle-drag  (list :editor :map) (root x y left-or-right)
  (if (equalp left-or-right :left)
      (with-slots* (chosentool chosenmap chosenlayer chosentile) root
	(let* ((tile-x (floor (/ x 50)))
	       (tile-y (floor (/ y 50)))
	       (tool-is-already-applied-here (get-in dragged-table (list tile-x tile-y))))
	  (unless tool-is-already-applied-here

	    (let ((result (funcall (fset:lookup qmapper.tools:*tools* chosentool) root x y tile-x tile-y chosentile)))
	      
	      (if result
		  (setf root result)))
	    (if (and tile-y
		     tile-x
		     dragged-table
		     (nth tile-x dragged-table))
		(setf (nth tile-y (nth tile-x dragged-table)) t)))))))

(defmultimethod handle-drag (list :editor :tileset) (root x y left-or-right)
  (if (equalp left-or-right :left)
      (with-slots* (tilesets chosentileset chosentile) root
	(let* ((tile-x (floor (/ x 50)))
	       (tile-y (floor (/ y 50)))
	       (tile (get-in (tileset-tiles (nth chosentileset tilesets)) (list tile-x tile-y))))
	  (setf chosentile tile)))))

(defun handle-kbd-event (event)
  (when event
    (when-let (fn (qmapper.engine_events:get-engine-lambda event))
      (funcall fn))
    (handle-kbd-event (queues:qpop qmapper.keyboard_loop:kbd-queue))))

(defun idle (renderer draw-queue)
  (sdl2:render-clear renderer)
  
  ;; (dolist (sprite draw-queue)
  ;;   (render sprite renderer))

  (render-scene renderer *document*)
  
  (sdl2:render-present renderer)
  (sleep 0.002)

  (when (equalp app-state :engine)
    (handle-kbd-event (queues:qpop qmapper.keyboard_loop:kbd-queue)))
  
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
  (let ((map (qmapper.root:root-get-chosen-map root)))
    (when map
      (let ((w (map-width map))
	    (h (map-height map)))
	(setf dragged-table (make-2d w h nil))))))

(defun stop-drag ()
  (setf dragged-table nil))

(defvar ctrl-down? nil)
(defvar alt-down? nil)
(defvar shift-down? nil)

(defun handle-kbd (keysym)
  (when (equalp app-state :engine)
    (let* ((scancode (sdl2:scancode-value keysym))
	   (sym (sdl2:sym-value keysym))
	   (mod-value (sdl2:mod-value keysym))
	   (name (sdl2:scancode-key-name scancode)))

      
      (cond ((substring? name "Ctrl") (setf ctrl-down? (not ctrl-down?)))
	    ((substring? name "Alt") (setf alt-down? (not alt-down?)))
	    ((substring? name "Shift") (setf shift-down? (not shift-down?)))
	    (t (queues:qpush qmapper.keyboard_loop:kbd-queue (format nil "~a~a~a~a"
								     (if ctrl-down? "C-" "")
								     (if alt-down? "M-" "")
								     (if shift-down? "S-" "")
								     name))))
      
      
      (format t "Key sym: ~a, code: ~a, mod: ~a, name: ~a~%"
	      sym
	      scancode
	      mod-value
	      name
	      ))))

(defun handle-windowevent ()
  (let ((flags (sdl2:get-window-flags *window*)))
    ;; haaaack
    (if (or (position :mouse-focus flags)
	    (position :input-focus flags))
	(progn
	  ;;got focus
	  (format t "We have a focus!~%"))
	(progn
	  ;;lost focus
	  (setf ctrl-down? nil
		alt-down? nil
		shift-down? nil)))))

(defun event-loop (renderer)
  ;;(push (qmapper.obj:create-sprite :texture-path "/home/feuer/Sync/qt-test/kaunis_tileset.jpeg" :renderer renderer) *draw-queue*)
  ;; (dotimes (i 20)
  ;;   (push (qmapper.obj:create-sprite :texture-path "/home/feuer/Sync/qt-test/kaunis_tileset.jpeg" :renderer *renderer*) *draw-queue*))
  (sdl2:with-event-loop (:method :poll)
    (:keydown (:keysym keysym)
	      (handle-kbd keysym))

    (:keyup (:keysym keysym)
	    (when (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
	      (sdl2:push-event :quit)))

    (:mousebuttondown ()
		      (start-drag *document*))
    (:mousebuttonup ()
		    (stop-drag))

    (:windowevent () 
		  (handle-windowevent))

    (:mousemotion (:x x :y y)
		  (handler-case 
		  (if (or (sdl2:mouse-state-p +left-mouse-button+)
			  (sdl2:mouse-state-p +right-mouse-button+))
		      (if (equalp app-state :editor)
			  
			  (setf qmapper.root:*document*
				(handle-drag qmapper.root:*document*
					     x y 
					     (cond ((sdl2:mouse-state-p +left-mouse-button+) :left)
						   ((sdl2:mouse-state-p +right-mouse-button+) :right)
						   (t nil))))
			  (setf qmapper.root:*engine-document*
				(handle-drag qmapper.root:*engine-document*
					     x y 
					     (cond ((sdl2:mouse-state-p +left-mouse-button+) :left)
						   ((sdl2:mouse-state-p +right-mouse-button+) :right)
						   (t nil))))))
		  (error (c)
		    (format t "tool-error: ~a~%" c))))
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
	(sdl2:set-render-draw-blend-mode renderer sdl2-ffi:+SDL-BLENDMODE-BLEND+)
	(setf *renderer* renderer)
	(setf *window* win)
	(event-loop renderer)))))

;; (main)

;; example/test code for loading a project
;; (schedule-once (lambda ()
;; 		 (setf *document*
;; 		       (load-doc! #P"/home/feuer/testi.sdlmap" *renderer*))))
    

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
