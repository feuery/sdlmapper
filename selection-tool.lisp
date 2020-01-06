(defpackage :qmapper.selection-tool
  (:use :common-lisp
	:qmapper.editor_events
	:qmapper.map
	:qmapper.tile
	:cl-arrows
	:qmapper.export
	:qmapper.root
	:qmapper.std)
  (:shadowing-import-from :fset :empty-map :with-first :with :seq :image :lookup ;; :filter
			  :reduce :size :concat :convert :wb-map-from-list))

(in-package :qmapper.selection-tool)

(defun get-points-in-selection (left top right bottom)
  (loop for x in (drop-last (minmax-range left right))
     append (loop for y in (drop-last (minmax-range top bottom))
	       collect (list x y))))

(defvar *clipboard* nil)



(add-key-lambda "C-X"
		(lambda ()
		  (let* ((selection (root-selected-coordinates *document*))
		  	 (left (fset:lookup selection 0))
		  	 (top (fset:lookup selection 1))
		  	 (right (fset:lookup selection 2))
		  	 (bottom  (fset:lookup selection 3))
		  	 (selected-tile-coord-pairs (get-points-in-selection left top right bottom))
		  	 (tiles (mapcar (lambda (coord-pair)
					  (fset:map ("tile" (get-tile-at2 (root-chosenLayerInd *document*) (car coord-pair) (cadr coord-pair)))
						    ("relative-coordinate" (list (- (car coord-pair) left)
										 (- (cadr coord-pair) top)))))
					selected-tile-coord-pairs)))
		    (format t "Eating tiles from ~a to ~a~%" (list left top) (list right bottom))
		    (setf *clipboard* (fset:map ("x" left)
						("y" right)
						("width" (- right left))
						("height" (- bottom top))
						("tiles" tiles)))
		    (set-doc (reduce (lambda (doc tile-coord-pair)
				       (let ((x (car tile-coord-pair))
					     (y (cadr tile-coord-pair)))
					 (set-tile-at-chosen-map doc x y (make-tile :x 0 :y 0 :tileset  0 :rotation 0 :gl-key nil))))
				     selected-tile-coord-pairs :initial-value *document*)))))

(add-key-lambda "C-V"
		(lambda ()
		  (if-let (selection (root-selected-coordinates *document*))
		      (let* ((left (fset:lookup selection 0))
		  	     (top (fset:lookup selection 1))
			     (tiles *clipboard*)
			     (real-tiles (lookup tiles "tiles"))
			     (width (lookup tiles "width"))
			     (height (lookup tiles "height")))
			(set-doc (reduce (lambda (doc coord-pair)
					   (let* ((x (+ left (car coord-pair)))
						  (y (+ top (cadr coord-pair))))
					     (if-let (tile (-> (remove-if-not (lambda (tile)
										(let ((relative-coords (lookup tile "relative-coordinate")))
										  (equalp relative-coords (list (- x left) (- y top)))))
									      real-tiles)
							       car
							       (lookup "tile")))
						 (progn
						   (format t "in C-V tile is ~a ~%" tile)
						   (set-tile-at-chosen-map doc x y tile))
					       (progn
						 (format t "in C-V tile is ~a :(~%" tile)
						 (format t "real-tiles are ~a~%" real-tiles)
						 doc))))
					 (loop for x in (mapcar #'dec (range width))
					    append (loop for y in (mapcar #'dec (range height))
						      collect (list x y)))
					 :initial-value *document*))
			(format t "resulting doc calculated~%"))
		    (format t "There's no selection!~%"))))
