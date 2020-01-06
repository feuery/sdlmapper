(defpackage :qmapper.resize-test
  (:use :common-lisp
	:qmapper.test-framework
   :cl-arrows
	:qmapper.std
   :qmapper.map
	:qmapper.layer
   :qmapper.root)
  
  (:import-from :fset :size :convert :empty-map :with))

(in-package :qmapper.resize-test)


(defvar grow-w (random 99))
(defvar grow-h (random 99))

(deftest testing-layer-growth
    (let ((qmapper.root:*document* (make-map :name-with-layers
					     :layers *document*
					     :sprites "Lol"
					     :animatedSprites2
					     :hit-layer 2
					     :scripts-to-run 1)))
    (let* ((w (random 44))
	   (h (random 66))
	   (new-w (+ w (random 23)))
	   (new-h (+ h (random 22)))
	   (new-layer (resize-layer (make-layer :name "Lol"
						:opacity 254
						:tiles (make-tiles w h)) new-w new-h)))
      ; (format t "old dims ~a, new expected dims ~a, new dims ~a~%" (list w h) (list new-w new-h) (list (layer-width new-layer) (layer-height new-layer)))
      
      (is (layer-width new-layer) new-w)
      (is (layer-height new-layer) new-h))))

(deftest testing-layer-shrink
    (let ((qmapper.root:*document* (make-map :name-with-layers
					     :layers *document*
					     :sprites "Lol"
					     :animatedSprites2
					     :hit-layer 2
					     :scripts-to-run 1))
	  (w (+ 50 (random 44)))
	  (h (+ 50 (random 66))))
      (assert (> w 50))
      (assert (> h 50))
      (let* ((new-w 50)
	     (new-h 50)
	     (new-layer (assert-some? (resize-layer (make-layer :name "Lol"
								:opacity 222
								:tiles (make-tiles w h)) new-w new-h))))
	(is (layer-width new-layer) new-w)
	(is (layer-height new-layer) new-h))))

(deftest testing-vertical-layer-growth
    (let ((qmapper.root:*document* (make-map :name-with-layers
					     :layers *document*
					     :sprites "Lol"
					     :animatedSprites2
					     :hit-layer 2
					     :scripts-to-run 1)))
      (let* ((w (random 44))
	     (h (random 66))
	     (new-layer (grow-layer-vertically (make-layer :name  "Lol"
							   :opacity 254
							   :visible t
							   :tiles (make-tiles w h))))
	     (result (layer-width new-layer))
	     (new-h (layer-height new-layer)))
	;; (format t "(equalp ~a ~a)?~%" result (inc w))
	;; (format t "(equalp ~a ~a)?~%" new-h (inc h))
	(is (inc h) new-h))))

(deftest testing-vertical-layer-shrink
    (let ((qmapper.root:*document* (make-map :name-with-layers
					     :layers *document*
					     :sprites "Lol"
					     :animatedSprites2
					     :hit-layer 2
					     :scripts-to-run 1)))
      (let* ((w (random 44))
	     (h (random 66))
	     (new-layer (assert-some? (shrink-layer-vertically (make-layer :name "sdfsf"
									   :opacity 222
									   :visible t
									   :tiles (make-tiles w h)))))
	     (new-h (layer-height new-layer)))
	(is (dec h) new-h))))

(deftest testing-horizontal-layer-growth
    (let* ((qmapper.root:*document* (make-map :name-with-layers
					      :layers *document*
					      :sprites "lal"
					      :animatedSprites2
					      :hit-layer 2
					      :scripts-to-run 1))
	 (w (random 66))
	 (h (random 55))
	 (new-layer (assert-some? (grow-layer-horizontally (make-layer :name "#sdfsdf"
								       :opacity 255
								       :visible t
								       :tiles (make-tiles w h)))))
	 (result-w (layer-width new-layer)))
    (is (inc w) result-w)))

(deftest testing-horizontal-layer-shrink
    (let* ((qmapper.root:*document* (make-map :name-with-layers
					      :layers *document*
					      :sprites "lal"
					      :animatedSprites2
					      :hit-layer 2
					      :scripts-to-run 1))
	 (w (random 66))
	 (h (random 55))
	 (new-layer (assert-some? (shrink-layer-horizontally (make-layer :name "#sdfsdf"
									 :opacity 255
									 :visible t
									 :tiles (make-tiles w h)))))
	 (result-w (layer-width new-layer)))
    (is (dec w) result-w)))

(deftest test-map-resize
    (let* ((qmapper.root:*document* (make-map :name-with-layers
					      :layers *document*
					      :sprites "lal"
					      :animatedSprites2
					      :hit-layer 2
					      :scripts-to-run 1))
	 (new-w (+ 3 (random 22)))
	 (new-h (+ 3 (random 22)))
	 (qmapper.root:*document* (resize-selected-map qmapper.root:*document* new-w new-h))
	 (selected-map (get-prop-in qmapper.root:*document* (list "MAPS" (root-chosenmap qmapper.root:*document*)))))
    (is (true-map-width selected-map) new-w)
    (is (true-map-height selected-map) new-h)))
	 

;; (deftest testing-resize
;;     (let* ((test-root (make-map-with-layers (init-root!) "Lol" 2 2 1))
;;     	   (qmapper.root:*document* test-root)
;;     	   (chosenmapid (root-chosenmap test-root))
;;     	   (map (assert-some? (get-prop-in test-root (list "MAPS" chosenmapid))))
;;     	   (old-w (true-map-width map))
;;     	   (old-h (true-map-height map))
;;     	   (test-root (assert-some? (resize-selected-map test-root grow-w grow-h)))
;;     	   (qmapper.root:*document* test-root)
;;     	   (map (assert-some? (get-prop-in test-root (list "MAPS" chosenmapid)))))
      
;;       (is (+ old-w grow-w) (true-map-width map))
;;       (is (+ old-h grow-h) (true-map-height map))))

;; (run-tests)
