(defpackage :qmapper.std
  (:use :common-lisp :cl-arrows)
  (:import-from :qmapper.export :defmacro-export! :defun-export! :defvar-export!)
  (:import-from :fset :empty-map :empty-seq :seq :insert :convert :with :lookup :wb-map-from-list :fset-setup-readtable)
  (:import-from :cl-ppcre :regex-replace-all :create-scanner :scan :parse-string)
  (:shadowing-import-from :cl-strings :replace-all)
  (:shadowing-import-from :fset :map :map? :seq?))

(in-package :qmapper.std)

(setf *readtable* (fset-setup-readtable *readtable*))
#+ecl
(setf compiler::*delete-files* nil)

;; (setf si::universal-error-handler (lambda (&rest args)
;; 				    (format "Mitä tää universal error handleri ottaa sisäänsä? ~a~%" args)))

(defun-export! dec (n)
  (- n 1))
(defun-export! inc (n)
  (+ n 1))

(defun-export! pos? (n)
  (> n 0))

(defun-export! neg? (n)
  (< n 0))

(defun-export! minmax-range (min max)
  (labels ((-range (min acc)
		 (if (equalp min max)
		     (cons min acc)
		     (-range (inc min) (cons min acc)))))
    (reverse (-range min '()))))


(defun-export! range (max)
  (labels ((-range (max acc)
	     (if (= 0 max)
		 acc
		 (-range (dec max) (cons max acc)))))
    (-range max '())))

(defun-export! slurp (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun empty? (l)
  (let ((len (cond ((or (fset:map? l) (fset:seq? l)) (fset:size l))
		   (t (length l)))))
    (equalp len 0)))

(defun not-empty? (l)
  (not (empty? l)))

(defun pair? (p)
  (and (consp p)
       (cdr p)))

(defun alist? (l)
  (and l
       (listp (car l))
       (listp (cdr l))
       (let ((l (remove-if #'pair? l)))
	 (empty? l))))

;; (equalp
;; (eval (read-from-string (prin1-to-string qmapper.root:*document*)))
;; ;; 

;; if this fails, we can't save and load documents
;; (fset:equal? (eval (read-from-string (prin1-to-string qmapper.root:*document*))) qmapper.root:*document*)
	     
(defun-export! good-gensym ()
  (format nil "~a" (gensym)))

(defun-export! fset-first (l)
  (fset:first l))
      
(defun-export! pairs (r1 r2)
  (loop for x in r1
	      append (loop for y in r2
			   collect (list x y))))

(defun-export! plist-get (lst key)
  ;; (declare (optimize (debug 3)))
  ;; (break)
  (if (not lst)
      '()
      (let ((list (drop-while (lambda (e)
				(if (not (symbolp e))
				    (let ((result 
					    (not (equalp e key))))
				      result)
				    (let* ((e (symbol-name  e))
					   (key (symbol-name key))
					   (result 
					     (not (equalp e key))))
				      result))) lst)))
	(if (not list)
	    '()
	    (car
	     (drop list
		   1))))))

(defun-export! partial (f &rest args)
  (lambda (&rest rst-args)
    (apply f (concatenate 'list args rst-args))))

(defun-export! repeatedly (fn n)
  (if (equalp n 0)
      '()
      (cons (funcall fn n) (repeatedly fn (dec n)))))

(defun-export! list-of (val n)
  (if (not (zerop n))
      (cons val (list-of val (dec n)))))

(defun-export! str (&rest strs)
  (apply (partial #'concatenate 'string)
	 (mapcar (lambda (o)
		   (write-to-string o :escape nil))
		   strs)))

(defun-export! drop (list n)
  (if (> n 0)
      (drop (cdr list)
	    (dec n))
      list))

(defun-export! alist->map (list)
  (wb-map-from-list list #'car #'cdr))

(defun-export! partition-by2 (pred lst)
  (reduce (lambda (acc e)
	    (if (funcall pred e)
		(let* ((s (car acc))
		       (lst (cadr acc))
		       (rst (drop acc 2)))
		  (cons s (cons (cons e lst)
				rst)))
		(cons e (cons acc '()))))
	  lst :initial-value '()))

(defun-export! take (list n)
  (labels ((-take (list n acc)
	     (if (and (car list)
		      (> n 0))
		 (-take (cdr list)
			(dec n)
			(cons (car list) acc))
		 acc)))
    (reverse (-take list n '()))))

(defun-export! take-while (pred coll)
  (labels ((-take-while (list acc)
	   (if (or (not list)
	       	   (not (funcall pred (car list))))
	       acc
	       (-take-while (cdr list)
	       		    (cons (car list) acc)))))
    (reverse (-take-while coll '()))))


(defun-export! partition-by (f coll)
  ;; (declare (optimize (debug 3)))
  (if coll
      (let* ((fst (first coll))
             (fv (funcall f fst))
             (run (cons fst (take-while (lambda (v)
					  (equalp fv (funcall f v)))
					(cdr coll))))
	     (run (if (= (length run) 1)
		      (car run)
		      run)))
        (cons run (partition-by f (drop coll (if (listp run)
						 (length run)
						 1)))))))

(defun-export! drop-last (seq)
  (reverse (cdr (reverse seq))))

(defun-export! drop-while (fn lst)
  ;; (declare (optimize (debug 3)))
  ;; (break)
  (if (or (not (funcall fn (car lst)))
	  (not lst))
      lst
      (drop-while fn (cdr lst))))

(defun-export! plist-get (lst key)
  ;; (declare (optimize (debug 3)))
  ;; (break)
  (if (not lst)
      '()
      (let ((list (drop-while (lambda (e)
				(if (not (symbolp e))
				    (let ((result 
					   (not (equalp e key))))
				      result)
				    (let* ((e (symbol-name  e))
					   (key (symbol-name key))
					   (result 
					    (not (equalp e key))))
				      result))) lst)))
	(if (not list)
	    '()
	    (car
	     (drop list
		   1))))))


(defun-export! slice (l offset n)
  (take (drop l offset) n))

(defun-export! assoc-to-ind (lst ind value)
  (if (< ind (length lst))
      (let ((beginning (slice lst 0 ind))
	    (end (drop lst (inc ind))))
	(concatenate 'list beginning (list value) end))
      (error  "Overflow in assoc-to-ind")))

(defvar *this* nil)
(defvar *call-logs* t)
(export '*call-logs*)
(export '*this*)

(defun-export! alist-cons (k v list)
  (format t "Älä kutsu alist-conssia ~%")
  (assert nil))
;;   (append (list (cons k v)) list))

(defun-export! sym (str)
  (let ((*read-eval* nil))
    (read-from-string str)))

(defun-export! zipmap (keys vals)
    (loop for k in keys
       for v in vals
       collecting (list k v)))

(defmethod fset:compare ((x symbol) (y symbol))
  (if (string= x y)
      :equal
      :unequal))

(defun-export! clean-key (key)
  (if (numberp key)
      key
      (string-upcase (replace-all (cond ((symbolp key) (symbol-name key))
					(t key)) "\"" ""))))

(defun-export! clean-val (val)
  (let* ((val (if (or (listp val)
		      (consp val))
		  (convert 'fset:seq val)
		  val))
	 (val (if (and (seq? val)
		       (any? #'listp val))
		  (progn
		    (fset:image (lambda (l)
				  (if (listp l)
				      (convert 'seq l)
				      l)) val))
		  (progn
		    val))))
    val))

;; (clean-val (seq (list 2 34) (list 12 3  5 6 43)))

(defun-export! valid-gl-key? (val)
  (let ((res (and (not (equalp val (empty-map)))
		  (not (equalp val (empty-seq))))))
    res))

(defun-export! get-prop  (obj-alist key)
  (handler-case 
      (let ((obj-alist (if (or (listp obj-alist)
			       (consp obj-alist))
			   (convert 'seq obj-alist)
			   obj-alist)))
	(if (numberp key)
	    (lookup obj-alist key)
	    (progn
	      (let* ((key (clean-key key))
		     (real-alist (convert 'list obj-alist))
		     (result (cdr (assoc key real-alist :test #'string=)))
		     (result (cond ((equalp result t) result)
				   ((and result
					 (symbolp result))
				    ;; if not for the prin1, this'd return rubbish strings to c...
				    (prin1-to-string (symbol-name result)))
				   (t result))))
		;; (when (equalp result (empty-seq))
		;;   (format t "WARN: returning empty seq for ~a~%" key))
		(values result
			key)))))
    (SIMPLE-ERROR (e)
      (format t "Kutsutaan ~a~%" (list 'get-prop obj-alist key))
      (error e))))

(defun-export! dissoc-prop (obj-alist key)
  (assert obj-alist)
  (fset:less obj-alist key))

(defun-export! dissoc-prop-in (obj-alist ks)
  (let* ((key (car ks))
	 (cdr? (equal 'cdr key))
	 (ks (cdr ks)))
    (if cdr?
	(if ks
	    (dissoc-prop-in (cdr obj-alist) ks)
	    (list (car obj-alist)))
	(if ks
	    (set-prop obj-alist key (dissoc-prop-in (get-prop obj-alist key) ks))
	    (dissoc-prop obj-alist key)))))
  

(defun-export! get-prop-in (obj ks)
  (values
   (let* ((key (car ks))
	  (cdr? (equal 'cdr key))
	  (ks (cdr ks)))
     (if cdr?
	 (if ks
	     (get-prop-in (cdr obj) ks)
	     (cdr obj))
	 (if ks
	     (get-prop-in (get-prop obj key) ks)
	     (get-prop obj key)))) ks))

(defun really-empty? (seq)
  (let ((s (fset:size seq)))
    (zerop s )))

(defun-export! any? (fn coll)
  (cond ((listp coll)
	 (if coll
	     (let ((res (funcall fn (car coll))))
	       (if (not res)
		   (any? fn (cdr coll))
		   res))))
	((seq? coll)
	 (if (not (fset:empty? coll))
	     (let ((res (funcall fn (fset:first coll))))
	       (if (not res)
		   (any? fn (fset:less-first coll))
		   res))))))
  

(defun-export! all? (fn coll)
  (not (any? (complement fn) coll)))

(defvar-export! *silence-validators* nil)

(defun-export! set-prop  (obj-alist key val)
  (let* ((obj-alist (if (or (listp obj-alist)
			    (consp obj-alist))
			(convert 'seq obj-alist)
			obj-alist))
	 (key (clean-key key))
	 (val (clean-val val))
	 (validator (get-prop-in *validators* (list (q-type-of obj-alist) key)))
	 (validator (if (consp validator)
			(eval validator)
			validator)))
    (assert (not (listp val)))
    (assert (not (consp val)))

    ;; (format t "Validator? ~a (~a)~%" validator (type-of validator))
    (if validator
	(if (funcall validator val obj-alist)
	    (let* ((new-obj (with (or obj-alist (empty-map)) key val))
		   (event-list (if (map? new-obj)
				   (get-prop-in new-obj (list "EVENTS" key))))
		   (result (if event-list
			       (reduce (lambda (obj fn)
					 (let ((fun (eval (read-from-string fn))))
					   (funcall fun obj val))) (convert 'list event-list) :initial-value new-obj)
			       new-obj)))
	      (unless result
		(format t "one of the event functions returned nil. You probably don't want that~%"))
	      result)
	    (progn
	      (unless *silence-validators*
		(format t "validator failed on obj ~a, key ~a and val ~a~%" obj-alist key val))
	      obj-alist))
	  
    (let* ((new-obj (with (or obj-alist (empty-map)) key val))
	   (event-list (if (map? new-obj)
			   (get-prop-in new-obj (list "EVENTS" key))))
	   (result (if event-list
		       (reduce (lambda (obj fn)
			     (let ((fun (eval (read-from-string fn))))
			       (funcall fun obj val))) (convert 'list event-list) :initial-value new-obj)
		       new-obj)))
      (unless result
	(format t "one of the event functions returned nil. You probably don't want that~%"))
      result))))

(defun-export! set-prop-in (obj ks value)
  (let* ((key (car ks))
	 (cdr? (equal 'cdr key))
	 (ks (cdr ks)))
    (if cdr?
	(if ks
	    (cons (car obj) (set-prop-in (cdr obj) ks value))
	    (cons (car obj) value))
	(if ks
	    (set-prop obj key (set-prop-in (get-prop obj key) ks value))
	    (set-prop obj key value)))))

(defun-export! update-prop (obj key fn)
  (set-prop obj key (funcall fn (get-prop obj key))))

(defun-export! update-prop-in (obj ks fn)
  (let* ((key (clean-key (car ks)))
	 (cdr? (equal 'cdr key))
	 (ks (cdr ks)))
    (if cdr?
	(if ks
	    (cons (car obj) (update-prop-in (cdr obj) ks fn))
	    (cons (car obj) (funcall fn (cdr obj))))
	(if ks
	    (set-prop obj key (update-prop-in (get-prop obj key) ks fn))
	    (update-prop obj key fn)))))

;; (update-prop-in (qmapper.map:make-map-with-layers "Lollo" 3 2 2)
;; 		'(layers 0 opacity)
;; 		#'inc)


(defun-export! add-event! (obj prop fn)
  (assert (functionp fn))
  (update-prop-in obj (list "EVENTS" (clean-key prop)) (lambda (l)
							 (let* ((l (or l (empty-seq))))
							   (insert l 0 (prin1-to-string (function-lambda-expression fn)))))))

(defvar-export! *validators* (map))

(defun create-symbol (sym)
  (assert (stringp sym))
  (read-from-string sym))

(defmacro-export! defcppclass (classname &rest rst)
  (let* ((visibility-stripped (apply #'append
				     (mapcan #'cdr rst)))
  	 (partitioned (partition-by #'listp visibility-stripped))
	 (props (plist-get partitioned 'properties))
	 (stupid-fields (concatenate 'list (plist-get partitioned 'fields)
			      props))
  	 (fields (reverse (mapcar (lambda (field)
				    (car field))
				  stupid-fields)))
	 (values (reverse (mapcar (lambda (field)
				    (second field))
				  stupid-fields)))
	 (validators (reverse (mapcar (lambda (field)
	 				(cons (prin1-to-string (car field)) (third field)))
	 			      stupid-fields)))
	 (fields-with-accessors (cons 'progn (concatenate 'list (->> stupid-fields
	 							     (mapcan (lambda (f)
	 								       (let* ((field (car f))
										      (getter (sym (str (symbol-name classname)
	 												"-"
	 												(symbol-name field))))
	 									      (setter (sym (str "set-"
	 												(symbol-name classname)
	 												"-"
	 												(symbol-name field)
	 												"!"))))
										 (list `(defun-export! ,getter (this)
											  (unless this
											    (format t "~a failed, this is nil~%" (prin1-to-string (quote ,getter)))
											    ;; Let's try to cause a core dump...
											    #+ecl (ext:quit -11)
											    #+sbcl (sb-ext:quit))
											  ;; (format t "this is ~a~%" this)
											  (when (or (consp this)
												    (integerp this))
											    (format t "*this* is funny: ~a~%" this)
											    )
											  (get-prop this ',field))
										       `(defun-export! ,setter (this val)
											  (assert this)
											  (set-prop this ',field val))))))))))
  	 (funcs (->> (plist-get partitioned 'functions)
		     (mapcar (lambda (fn)
			       (let ((name (sym (str (symbol-name classname)
						     "-"
						     (symbol-name (first fn)))))
				     (param-list (second fn))
				     (body (drop fn 2))
				     (this (sym (format nil "~a" (gensym)))))
				 `(defun-export! ,name (,this ,@param-list)
				    (let ((*this* ,this))
				      ,@body)))))))
	 (ctr-name (sym (str "make-" (symbol-name classname))))
	 (meta-name (sym (str (symbol-name classname) "-meta")))
	 (ctr `(defun-export! ,ctr-name (&key ,@(->> stupid-fields
						     reverse
						     (mapcar (lambda (f-v-validator)
							       (let ((f (car f-v-validator))
								     (value (second f-v-validator)))
								 (list f value))))))
		 (let* ((hash (empty-map))
			(fields (list "id" "events" ,@(mapcar (lambda (f)
						      (format nil "~a" f))
		 				fields)))
			(values (list (good-gensym) (empty-map) ,@fields))
			(fields-and-vals (cons (cons "TYPE-NAME" (list ,(symbol-name classname)))
					       (zipmap fields values))))

		   (reduce (lambda (hashmap k-v)
			     (let ((k (car k-v))
				   (v (cadr k-v)))
			       (set-prop hashmap k v))) fields-and-vals :initial-value hash))))
	 (meta-name (sym (str (symbol-name classname) "-meta")))
	 (meta-fn `(->> (reduce (lambda (map val)
				  (if (cdr val)
				      (set-prop map (car val) (cdr val))
				      map))
				(mapcar (lambda (prop)
					  (cons (car prop) (third prop)))
					',props) :initial-value (empty-map))))
	 (validate-fn `(defun-export! ,(create-symbol (format nil "validate-~a" classname)) (obj)
			 (let* ((type (q-type-of obj))
				(validator-map (convert 'list (get-prop *validators* type))))
			   (reduce (lambda (last-result
					    validator-kv)
				     (let* ((key (car validator-kv))
					    (validator (cdr validator-kv))
					    (validator (if (consp validator)
							   (eval validator)
							   validator)))
				       (if validator
					   (and last-result
						(funcall validator (get-prop obj key) obj))
					   last-result)))
				   validator-map :initial-value t ))))
	 )
    `(progn
       ,ctr
       ,fields-with-accessors
       ,@funcs
       ,validate-fn
       
       (setf *validators* (with *validators* (symbol-name ',classname) (convert 'map (read-from-string ,(prin1-to-string validators)))))

       (defun-export! ,meta-name ()
	 ,meta-fn)
       
       (symbol-name ',classname))))

(defun-export! q-type-of  (obj-alist)
  ;; (format t "obj-alist on ~a~%" obj-alist)
  (handler-case
      (get-prop obj-alist "type-name")
    (TYPE-ERROR (te)
      (format t "Mikä tää type error on? ~a~%" te))
    (SIMPLE-TYPE-ERROR (ste)
      (format t "ste bongattu, obj-alist on ~a~%" obj-alist))
    (SIMPLE-ERROR (lol)
      (format t "Virhe: obj-alist on ~a~%" (prin1-to-string obj-alist))
      "NULL-TYPE")))

(defun-export! q-type-of-2 (obj)
  "If the object isn't compatible with the qmapper object model, it's dumped to cl's type-of"
  (handler-case
      (get-prop obj "type-name")
    (SIMPLE-TYPE-ERROR (ste)
      (format t "ste bongattu, obj on ~a~%" obj))
    (SIMPLE-ERROR (lol)
      (type-of obj))))

;; (delete-duplicates (concatenate 'list (range 10) (range 12)) :test #'equalp)

(defun-export! keys  (a)
  (delete-duplicates
   (mapcar #'car a) :test #'equalp))

(defun-export! fset-keys-str (seq-or-map)
  (cond ((map? seq-or-map) (fset-map-keys seq-or-map))
	((seq? seq-or-map) (range (fset:size seq-or-map)))
	(t (error "Unrecognized argument in fset-keys-str ~a" seq-or-map))))
  

(defun contains? (key list)
  (remove-if-not (lambda (l) (equalp l key)) list))

(defun-export! fset-map-keys (m)
  (convert 'fset:seq (remove-if #'not (keys (convert 'list m)))))

(defun-export! keys-str  (a)
  (let ((result (mapcar #'clean-key (keys (convert 'list a)))))
    result))

(defun-export! prop-list?  (alist k)
  (listp (get-prop alist k)))

(defun-export! prop-str?  (alist k)
  (stringp (get-prop alist k)))

(defun-export! prop-number?  (alist k)
  (numberp (get-prop alist k)))

(defun-export! prop-bool?  (alist k)
  (let ((v (get-prop alist k)))
    (or (equalp v  t)
	(equalp v nil))))

(defun-export! prop-float?  (alist k)
  (let ((v (get-prop alist k)))
    (floatp v)))

(defun-export! prop-sym?  (alist k)
  (symbolp (get-prop alist k)))

;; (tree-il->scheme
;;  (macroexpand 
;;   '(defcppclass Layer
;; 		 (public
;; 		  (properties
;; 		   (std__string name "")
;; 		   (unsigned_char opacity 255)
;; 		   (bool visible #t)
;; 		   (std__vector<std__vector<Tile*>>* tiles '())
;; 		   (Map* parent nil))
;; 		  (functions
;; 		   (width ()
;; 			  (length (first (Layer-tiles (this)))))
;; 		   (height ()
;; 			   (length (caar (Layer-tiles (this))))))))))



;; (layer- (make-Layer 'p '(1 2 3) #t 244 "Layer")

(defmacro-export! condp (bin-op param1 &rest rst)
  "This is supposed to work like clojure's condp. This evaluates param1 only once.
(condp equalp \"Lollero ooo\"
       \"Lol\" 3
       \"Lollero\" 4
       'else) => else
and 

(condp equalp \"Lollero\"
       \"Lol\" 3
       \"Lollero\" 4) => 4"
  (let ((p1 (gensym)))
    (labels ((emit (params)
	       (if (equalp (length params) 1)
		   (car params)
		   (if params
		       (let ((else (emit (cddr params))))
			 `(if (,bin-op ,p1 ,(car params))
			      ,(cadr params)
			      ,else))))))
      `(let ((,p1 ,param1))
	 ,(emit rst)))))

(defmacro-export! if-let (bindings then &optional else)
  `(let (,bindings)
     (if ,(car bindings)
	 ,then
	 ,else)))

;; (condp equalp "Lollero"
       
;;        "Lol" 3
;;        "Lollero" 4
;;        'pröp) => 4

;; (condp equalp "Lollero"
;;        "Lol" 3
;;        "Lollero" 4) => 4

(defun-export! get-list-index  (l pred)
  (if (not l)
      -1
      (if (funcall pred (car l))
          0
          (let ((result (get-list-index (cdr l) pred)))
            (if (= result -1)
                -1
                (1+ result))))))

(defun-export! drop-plist-keys  (lst)
  (labels ((dropper (lst acc count)
	     (if (not lst)
		 acc
		 (if (oddp count)
		     (dropper (cdr lst)
			      (cons (car lst) acc)
			      (inc count))
		     (dropper (cdr lst)
			      acc
			      (inc count))))))
    (reverse (dropper lst '() 0))))

(defun-export! drop-alist-keys (lst)
  (labels ((dropper2 (lst acc)
	     (if (not lst)
		 acc
		 (dropper2 (cdr lst)
			   (cons (cdar lst) acc)))))
    (reverse (dropper2 lst '()))))

;; (drop-alist-keys `((AAA . 3) (BEE 2))) => '(3 2)

;; (drop-plist-keys '(:lol 1 :lollo 3 'asd 33))  => '(1 3 33)

(defun-export! distance  (x1 y1 x2 y2)
  (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1)))))

(defun-export! distance-pairs  (xy1 xy2)
  (distance (car xy1) (cadr xy1)
	    (car xy2) (cadr xy2)))

(defun-export! sort-by  (keyfn coll)
  (sort coll (lambda (x y)
	       (< (funcall keyfn x) (funcall keyfn y)))))

;; (sort-by #'car (list (list 3 1 2 22) (list 1 2 3) (list 5 4 22)))

;; (find-nearest 1 1 '((3 3) (50 2) (0 1)))  => (0 1)

(defun-export! sublist  (list max-ind)
  (if (<= max-ind 0)
      '()
      (cons (car list)
	    (sublist (cdr list)
		     (dec max-ind)))))

(defun-export! sublist-from  (list min-ind)
  (drop list (inc min-ind)))

(defun-export! drop-list-i  (lst index)
  (concatenate 'list
	       (sublist lst index)
	       (sublist-from lst index)))

(defun-export! drop-alist-keys (alist)
  (mapcar #'cdr alist))

(defun-export! filter (l s)
  (remove-if-not l s))

(defun-export! to-list (o)
  ;; (format t "to-list ~a~%" o)
  (convert 'list o))

(defun mapcat (fn coll)
  (apply #'concatenate (list 'list (apply #'mapcar fn coll))))

(defun-export! flatten-get (2dlist prop)
  (mapcat (lambda (e)
	    (get-prop e prop)) 2dlist))


(defun-export! set-timeout  (msecs fn)
  #+ecl(let ((proc (mp:make-process :name (str "timeout fn" (random 2222))))
	     (f (lambda (lol)
		  (sleep (/ msecs 1000))
		  (funcall fn))))
	 (mp:process-preset proc f nil)
	 (mp:process-enable proc)))

(defvar *loops-running* t
  "ECL's process api doesn't provide a working way to stop a process. Thus you have to kill a process 
by setting this var to nil and killing every process on the way. TODO make a better api with qthreads")

(defun-export! qloop (fn)
  (set-timeout 400 (lambda ()
		     (funcall fn)
		     (when *loops-running*
		       (qloop fn)))))

(defun-export! key-down? (key)
  (funcall qmapper.export:keydown key))

(defmacro-export! while (condition &body body)
  `(loop while ,condition
      do (progn ,@body)))

(defun-export! drop-seq (src seq-to-drop)
  (format t "Dropping seq, ~a~%" (fset:map ("SRC" src)
					 ("SEQ-TO-DROP" seq-to-drop)))
  (let ((src (if (listp src)
		 (fset:convert 'fset:seq src)
		 src))
	(seq-to-drop (if (listp seq-to-drop)
			 (fset:convert 'fset:seq seq-to-drop)
			 seq-to-drop)))
    (fset:remove-if (lambda (n) (fset:contains? (fset:convert 'fset:set seq-to-drop) n))
		    src)))

;; (qloop (lambda ()
;; 	 (when (key-down? "KEY-DOWN")
;; 	   (format t "Painoit alanappia!~%"))))

;; (defvar proc (qloop (lambda ()
;; 		      (format t "AAAAA~%"))))

;; Testikoodi enginelle:

;; (add-event "2001100545" "Sprite" "X" (lambda (id)
;; 				       (let ((x (get-prop id "Sprite" "x")))
;; 					 (if (> x 200)
;; 					     (scm-puts "Voitit pelin")
;; 					     (begin
;; 					       (scm-puts "Lol äxä on: ")
;; 					       (scm-puts (object->string x)))))))


;; (qloop (lambda ()
;; 	 (when (key-down? 'Key_Right)
;; 	   (let ((x (get-prop "2001100545" "Sprite" "x")))
;; 	     (set-prop "2001100545" "Sprite" "x" (+ x 10))))))


;; (export-all :qmapper.std)
