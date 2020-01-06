(defpackage :qmapper.tileset
  (:use :common-lisp
	:cl-arrows
	:qmapper.std
	:qmapper.export
	:qmapper.root
	:qmapper.tile)
  (:import-from :fset :lookup :with :empty-map)
  (:shadowing-import-from :fset :convert)
  (:import-from :qmapper.export
		:explode :set-img-rotation
		:load-image :image-w :image-h :copy-image
		:add-to-drawingqueue  :clear-drawingqueue
		:set-img-x :set-img-y
		:do-schedule-lambda))		

(in-package :qmapper.tileset)


(defcppclass Tileset
    (public
     (fields
      (name "Tileset 1")
      ;; (vertexShader nullptr)
      ;; (fragmentShader nullptr)
      (tiles '())
      (w 0) 				; in tiles
      (h 0))))				; in tiles

(defun-export! get-tile (tileset x y)
  (get-prop-in (tileset-tiles tileset) (list x y)))

(defun-export! select-tileset-to-draw (*this*)
  (clear-draw-queue :TILESET)
  (let ((coords (pairs (mapcar #'dec (range (Tileset-w *this*)))
		       (mapcar #'dec (range (Tileset-h *this*)))))
	(tiles (Tileset-tiles *this*)))
    ;; (format t "coord-pairs are ~a~%w and h are ~a & ~a ~%" coords
    ;; 	    (Tileset-w *this*)
    ;; 	    (Tileset-h *this*))
    (dolist (c-pair coords)
      (let* ((x (car c-pair))
      	     (y (cadr c-pair))
      	     (img (get-prop-in tiles (list x y))))
	(if img
	    (progn
              (set-image-x :TILESET img (truncate (* x 50.0)))
              (set-image-y :TILESET img (truncate (* y 50.0)))
              (add-to-drawqueue img :TILESET))
	    (progn
	      (format t "x and y are [~a ~a]~%" x y)))))
    (format t "dolist is done~%")))

(defun-export! select-tileset (tileset)
  (format t "Selecting tileset~%")
  (select-tileset-to-draw tileset)
  (assert (not (consp tileset))))
  				 
(defun-export! load-tilesetless-texture-splitted (path &key (tile-width 50) (tile-height 50))
  (let* ((root-img (load-img path))
	 (_ (format t "img loaded~%"))
  	 (w (floor (/ (img-width root-img) tile-width)))
  	 (h (floor (/ (img-height root-img) tile-height)))
	 (_ (format t "dimensions ~a found~%" (list w h)))
	 (textures (mapcar (lambda (x)
  			     (mapcar (lambda (y)  
  				       (copy-img root-img
  						 (* x tile-width) (* y tile-height)
  						 tile-width tile-height))
  				     (mapcar #'dec (range h))))
  			   (mapcar #'dec (range w)))))
    (format t "textures ~a loaded!~%" textures)
    (values
     textures
     w
     h)))	      

(defun-export! load-texture-splitted (path)
  (let* ((root-img (load-img path))
	 (_ (format t "img loaded~%"))
  	 (w (/ (img-width root-img) 50))
  	 (h (/ (img-height root-img) 50))
	 (_ (format t "dimensions found~%"))
	 (textures (mapcar (lambda (x)
  			     (mapcar (lambda (y)
				       (make-tile :x x :y y :tileset (tileset-count!) :rotation 0
						  :gl-key
  						  (copy-img root-img
  							    (* x 50) (* y 50)
  							    50 50)))
  				     (mapcar #'dec (range h))))
  			   (mapcar #'dec (range w)))))
    (format t "textures loaded!")
    (values
     textures
     w
     h)))

(defun-export! push-tileset (root tileset)
  (let* ((tilesets (set-prop (root-tilesets root)
			     (get-prop tileset "ID")
			     tileset))
	 (final-root (set-root-tilesets! root tilesets)))
    final-root))

(defun-export! load-tileset (path)
  (format t "Going into load-tileset~%")
  (schedule-once :TILESET (lambda ()
			    (multiple-value-bind (tiles w h) (load-texture-splitted path)
			      (format t "Loaded tile textures, going to make tileset ~%")
			      (let* ((tileset (make-tileset :name "New tileset" :tiles tiles :w w :h h))
				     (result (push-tileset *document* tileset)))
				(format t "set-docing result~%")
				(set-doc result ))))))
