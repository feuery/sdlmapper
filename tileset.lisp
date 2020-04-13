(defpackage :qmapper.tileset
  (:use :common-lisp
	:qmapper.obj
	:cl-arrows
	:qmapper.obj
	:qmapper.std
	:qmapper.obj
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
		:do-schedule-lambda)
  (:export :tileset :tileset-tiles))

(in-package :qmapper.tileset)

(defvar *amount-of-tilesets* 0)

(defclass tileset ()
  ((name :accessor tileset-name :initform (format nil "Tileset ~a" *amount-of-tilesets*))
   (tiles :accessor tileset-tiles)
   (width :accessor tileset-width)
   (height :accessor tileset-height)))


;; (defcppclass Tileset
;;     (public
;;      (fields
;;       (name "Tileset 1")
;;       ;; (vertexShader nullptr)
;;       ;; (fragmentShader nullptr)
;;       (tiles '())
;;       (w 0) 				; in tiles
;;       (h 0))))
					; in tiles

(defun-export! get-tile (tileset x y)
  (get-in (tileset-tiles tileset) (list x y)))
  

(defun-export! select-tileset-to-draw (*this*)
  (clear-draw-queue)
  (let ((coords (pairs (mapcar #'dec (range (tileset-width *this*)))
		       (mapcar #'dec (range (tileset-height *this*)))))
	(tiles (tileset-tiles *this*)))
    ;; (format t "coord-pairs are ~a~%w and h are ~a & ~a ~%" coords
    ;; 	    (Tileset-w *this*)
    ;; 	    (Tileset-h *this*))
    (dolist (c-pair coords)
      (let* ((x (car c-pair))
      	     (y (cadr c-pair))
      	     (img (get-prop-in tiles (list x y))))
	(if img
	    (progn
	      (with-slots (qmapper.obj:position) *this*
		(setf position (list (truncate (* x 50.0))
				     (truncate (* y 50.0)))))
              (add-to-drawqueue img))
	    (progn
	      (format t "x and y are [~a ~a]~%" x y)))))
    (format t "dolist is done~%")))

(defun-export! select-tileset (tileset)
  (format t "Selecting tileset~%")
  (select-tileset-to-draw tileset)
  (assert (not (consp tileset))))
  				 
(defun-export! load-tilesetless-texture-splitted (path &key renderer (tile-width 50) (tile-height 50))
  (assert renderer)
  (let* ((root-sprite (create-sprite :texture-path path))
	 (root-size (sprite-size root-sprite))

  	 (w (floor (/ (first root-size) tile-width)))
  	 (h (floor (/ (second root-size) tile-height)))

	 (textures (mapcar (lambda (x)
  			     (mapcar (lambda (y)  
  				       (create-subsprite root-sprite (list
								   (* x tile-width) (* y tile-height)
								   tile-width tile-height)
							 renderer))
  				     (mapcar #'dec (range h))))
  			   (mapcar #'dec (range w)))))
    (format t "textures ~a loaded!~%" textures)
    (values
     textures
     w
     h)))

(defun-export! load-texture-splitted (path renderer)
  (let* ((root-img (create-sprite :texture-path path :renderer renderer))
	 (size (sprite-size root-img)))
    (assert (> (first size) 0))
    (let* ((w (/ (first size) 50))
	   (h (/ (second size) 50))
	   (textures (mapcar (lambda (x)
			       (mapcar (lambda (y)
					 (make-instance 'qmapper.tile:tile :x x :y y :tileset (tileset-count!) :rotation 0
						    :sprite
						    (create-subsprite root-img
								      (list (* x 50) (* y 50)
									    50 50)
								      renderer)))
				       (mapcar #'dec (range h))))
			     (mapcar #'dec (range w)))))
      (format t "textures loaded!")
      (values
       textures
       w
       h))))

(defun-export! load-tileset (path)
  (format t "Going into load-tileset~%")
  (multiple-value-bind (tiles w h) (load-texture-splitted path)
    (format t "Tiles: ~a~%" tiles)
    (format t "Loaded tile textures, going to make tileset ~%")
    (let* ((tileset (make-tileset :name "New tileset" :tiles tiles :w w :h h))
	   (result (push-tileset *document* tileset)))
      (format t "set-docing result~%")
      (set-doc result ))))

(defmethod initialize-instance :after ((tset tileset) &key renderer tileset-path)
  (assert renderer)
  (assert tileset-path)
  (multiple-value-bind (loaded-tiles loaded-width loaded-height) (load-texture-splitted tileset-path renderer)
    ;; TODO this could be macrofied. Could with-slots be made immutable?
    (with-slots (tiles width height) tset
      (setf tiles loaded-tiles)
      (setf width loaded-width)
      (setf height loaded-height))))

(defmethod draw ((tset tileset) &key renderer (x 0) (y 0))

  (with-slots (width height tiles) tset
    (let* ((xs (mapcar #'dec (range width)))
	   (ys (mapcar #'dec (range height)))
	   ;; those ^^ are the indexes to tiles
	   (pairs (pairs xs ys)))
      ;; TODO this loop could be optimized by keeping a hold of the original surface and drawing it instead of tiles
      (dolist (pair pairs)
	(let* ((x-index (car pair))
	       (y-index (cadr pair))
	       (tile-obj (get-in tiles pair)))
	  (draw tile-obj :renderer renderer
		:x (+ x (* x-index 50))
		:y (+ y (* y-index 50))))))))
	    
  ;; ladataan tile-tekstuuri muodossa jossa me saadaan yksittäinen 50x50 - tile viittaamalla siihen [x][y]
  ;; sen jälkeen asetetaan tilesetin width ja height tileissä
  ;; ja heitetään takaisin kutsujalle
