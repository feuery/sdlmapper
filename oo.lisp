(defpackage :qmapper.oo-test
  (:use :common-lisp
	:multimethods
	:qmapper.std
	:qmapper.test-framework))

(in-package :qmapper.oo-test)

(setf *readtable* (fset:fset-setup-readtable *readtable*))

(defclass* test-class2
    (a "lol")
  (b 12))

(defclass* test-class
    (slot-1 "AA")
  (slot-2 (make-test-class2 :b 24)))

(deftest how-do-side-effects-work
    (is 
     (let ((side-effect (lambda (param)
			  (with-slots* (b) param
			    (setf b "This is updated too :D")
			    (- 23 2)))))
       (let* ((b (make-test-class2))
	      (a (make-test-class :slot-2 b)))
	 (with-slots* (slot-1 slot-2) a
	   (setf slot-2 (funcall side-effect slot-2))
	   ;;(funcall side-effect slot-2)
	   (setf slot-1 "UPDATED"))))
     (fset:map
      ("TYPE" 'TEST-CLASS)
      ("SLOT-1" "UPDATED")
      ("SLOT-2" (fset:map ("A" "lol") ("B" "This is updated too :D") ("TYPE" 'TEST-CLASS2)))))

  (is (let ((a (make-test-class :slot-1 (list 1 2 3)
				:slot-2 (make-test-class2 :b (range 10)))))
	(with-slots* (slot-1 slot-2) a
	  (setf (nth 1 slot-1) "UPDATED")
	  (setf slot-2 (with-slots* (b) slot-2
			 (setf (nth 1 b) "REALLY UPDATED")))))
      (fset:map 
       ("TYPE" 'TEST-CLASS)
       ("SLOT-1" (list 1 "UPDATED" 3))
       ("SLOT-2"
	(fset:map
	 ("A" "lol")
	 ("B" (list 1 "REALLY UPDATED" 3 4 5 6 7 8 9 10))
	 ("TYPE" 'TEST-CLASS2))))))

(defmulti test-dispatch #'equalp (val)
	  (fset:lookup val "TYPE"))

(defmultimethod test-dispatch 'test-class2 (val)
		"test-class 2")

(defmultimethod test-dispatch 'test-class (val)
		"test-class")

(deftest do-test-dispatch
    (let ((obj (make-test-class))
	  (result nil))
      ;; with-slots* by definition returns the updated fset:map, and deftest expects this to return :pass if successfull
      (with-slots* (slot-1) obj
	(setf slot-1 "DADAA")
	(setf result
	      (and (is (test-dispatch obj)
		       "test-class")
		   (is (test-class-slot-1 obj)
		       "DADAA"))))
      result))

;;(run-tests)
