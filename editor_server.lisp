(defpackage :qmapper.editor-server
  (:use :common-lisp
	:qmapper.app-state
	:qmapper.animatedsprite
	:qmapper.sprite
	:qmapper.tools
        :qmapper.std
	:qmapper.map
	:qmapper.layer
	:qmapper.root
	:qmapper.tileset
	:qmapper.doc-server
        :ppcre
	:cl-arrows
        :usocket
	:bordeaux-threads)
  (:import-from :qmapper.export :defun-export!)
  (:shadowing-import-from :fset :empty-map :empty-seq :seq :insert :convert :with :lookup :wb-map-from-list :fset-setup-readtable :map)
  (:export :*sdl-single-command-queue* :schedule-once))

(in-package :qmapper.editor-server)

(setf *readtable* (fset-setup-readtable *readtable*))

(defparameter *sdl-single-command-queue* ())

(defun schedule-once (cmd)
  (push cmd *sdl-single-command-queue*))

(defparameter *server-running?* t)

(defun clean-map (m)
  (->> m
       (mapcar (lambda (alist-cell)
		 (cons (sb-mop:slot-definition-name (car alist-cell))
		       (cdr alist-cell))))
       (mapcar (lambda (alist-cell)
		 (if (equalp (car alist-cell) 'layers)
		     (cons 'layers (mapcar
				    #'layer-name
				    (cdr alist-cell)))
		     alist-cell)))))

(defun find-map-index (set-to-find-from
		       ;; the lambda that transforms element of set-to-find-from to its id
		       ;; otherwise I'd just do (with-slots (id) element), but CL's symbols are
		       ;; namespaced and stupid and don't work that way
		       id-getter
		       searched-id)
  (->> (range (length set-to-find-from))
       (mapcar #'dec)
       (remove-if-not (lambda (index)
			(let ((id (funcall id-getter (nth index set-to-find-from))))
			  (equalp searched-id id))))
       car))

(defun get-obj (root type searched-id)
  (let ((objs 
	 (cond ((equalp type "TILESET") (root-tilesets root))
	       ((equalp type "MAP") (root-maps root))
	       ;; TODO layerit on tosiaan siellä mapin alla, joten tää vaatii special casen
	       ((equalp type "LAYER") (root-layers root))
	       (t (error (format nil "Didn't recognize ~a" type))))))
    (->> objs
	 (remove-if-not (lambda (obj)
			  (with-slots* (id) obj
			    (equalp id searched-id))))
	 car)))

(defun get-layer-obj (root map-id layer-id)
  (let ((map (get-obj root "MAP" map-id)))
    (format t "map: ~a ~%" map)
    (->> map
	 map-layers
	 (remove-if-not (lambda (obj)
			  (let ((id (layer-id obj)))
			    (equalp id  layer-id))))
	 car)))
	

;; TODO replace this massive map with a qmapper.tools:deftool lookalike

(defparameter *message->event* (map))

(defmacro defmessage (message param-list &rest body)
  `(setf *message->event* (fset:with *message->event* ,message (lambda ,param-list
								 ,@body))))

(defmessage "LIST-TILESETS" (message client-socket params)
  (format (socket-stream client-socket)
	  (with-slots* (tilesets) *document* :read-only
		       (->> (zipmap 
			     (mapcar #'tileset-id tilesets)
			     (mapcar #'tileset-name tilesets))
			    (format nil "(~{~s~^~%~})")))))

(defmessage "LIST-MAPS"  (message client-socket params)
  (format (socket-stream client-socket) "~a~%"
	  (with-slots* (qmapper.root:maps) *document* :read-only
		       (mapcar (lambda (map)
				 (list (prin1-to-string (map-id map))
				       (prin1-to-string (map-name map))))
			       qmapper.root:maps))))
(defmessage "CREATE-MAP" (message client-socket params)
  (let ((map-w (parse-integer (car params)))
	(map-h (parse-integer (cadr params))))
    (setf *document*
	  (with-slots* (qmapper.root:maps) *document*
		       (push (-> (make-map)
				 (init-map :layer-w map-w :layer-h map-h :layer-count 1))
			     qmapper.root:maps)))))

(defmessage "SELECT-TILESET" (message client-socket params)
  (let* ((searched-id (parse-integer (car params)))
	 (index (->> (range (length (root-tilesets *document*)))
		     (mapcar #'dec)
		     (remove-if-not (lambda (index)
				      (with-slots* (qmapper.tileset:id) (nth index (root-tilesets *document*)) :read-only
						   (equalp searched-id qmapper.tileset:id)))))))
    (when index
      (setf *document* (with-slots* (chosentileset) *document*
				    (setf chosentileset (car index))))
      (setf qmapper.app-state:editor-state :tileset))))

(defmessage "SELECT-MAP" (message client-socket params)
  (let* ((searched-id (parse-integer (car params)))
	 (index (find-map-index (root-maps *document*) #'map-id searched-id)))
    (when index
      (setf *document* (with-slots* (chosenmap) *document*
			 (setf chosenmap index)))
      (setf qmapper.app-state:editor-state :map))))

(defmessage "LOAD-TILESET" (message client-socket params)
  (let* ((tileset-path (car params))
	 (tileset-name (cadr params)))
    (schedule-once (lambda ()
		     (setf *document*
			   (with-slots* (tilesets) *document*
					(push
					 (-> (make-tileset
					      :name tileset-name)
					     (init-tileset 
					      :tileset-path tileset-path :renderer *renderer*))
					 tilesets)))))))

(defmessage "LIST-TOOLS" (message client-socket params)
  (->> *tools*
       (fset:convert 'list)
       (mapcar #'car)
       (mapcar (lambda (tool-kw)
		 ;; emacs frontend expects id-name pairs
		 (list tool-kw tool-kw)))
       prin1-to-string
       (format (socket-stream client-socket) "~a~%")))

(defmessage "SELECT-TOOL" (message client-socket params)
  (let ((new-tool (read-from-string (car params))))
    (setf *document* (with-slots* (chosentool) *document*
		       (setf chosentool new-tool))))) 

(defmessage "LIST-LAYERS" (message client-socket params)
  (format t "params in list-layers: ~a~%" params)
  (if (pos? (length params))
      (let* ((chosenmap-index (find-map-index (root-maps *document*) #'map-id (parse-integer (car params))))
	     ;;(_ (format t "(nth ~a ~a)~%" chosenmap-index (root-maps *document*)))
	     (chosenmap (nth chosenmap-index (root-maps *document*))))
	(with-slots* (layers) chosenmap
		     (let ((inner-layers (->> layers
					(mapcar (lambda (l)
						  (list (layer-id l) (layer-name l)))))))
		       (format t "layers going to emacs: ~a~%" inner-layers)
		       (format (socket-stream client-socket) "~a~%" (prin1-to-string inner-layers)))))
      (let* ((chosenmap (root-get-chosen-map *document*)))
	(with-slots* (layers) chosenmap
		     (let ((inner-layers (->> layers
					(mapcar (lambda (l)
						  (list (layer-id l) (layer-name l)))))))
		       (format (socket-stream client-socket) "~a~%" (prin1-to-string inner-layers)))))))

(defmessage "CREATE-LAYER" (message client-socket params)
  ;; FIXME niin hyvää copypastaa
  ;;(format t "params in create-layer: ~a~%" params)
  (setf *document* 
	(if (pos? (length params))
	    (with-slots* (chosenlayer maps) *document*
			 (let* ((map-id (car params))
				(chosenmap-index (find-map-index (root-maps *document*) #'map-id (parse-integer map-id)))
				(chosenmap (nth chosenmap-index maps))
				(amount-of-layers (length (map-layers chosenmap))))
			   
			   (push-layer chosenmap)
			   (setf chosenlayer amount-of-layers)))							  
	    (with-slots* (chosenlayer) *document*
			 (let* ((chosenmap (root-get-chosen-map *document*))
				(amount-of-layers (length (map-layers chosenmap))))
			   (push-layer chosenmap)
			   (setf chosenlayer amount-of-layers))))))

(defmessage "SELECT-LAYER" (message client-socket params)
  (format t "params in select-layer: ~a ~%" params)
  (setf *document*
	(cond ((equalp (length params) 2)
	       (with-slots* (chosenmap chosenlayer maps) *document*
			    (let* ((new-map-index (find-map-index maps #'map-id (parse-integer (car params))))
				   (map (nth new-map-index maps)))
			      (setf (nth new-map-index maps)
				    (with-slots* (layers) map
						 (let ((layer-index (find-map-index layers #'layer-id (parse-integer (cadr params)))))
						   (setf chosenmap new-map-index)
						   (setf chosenlayer layer-index)))))))
	       ((equalp (length params) 1)
		(with-slots* (chosenmap chosenlayer maps) *document*
			     (let* ((map (root-get-chosen-map *document*)))
			       (with-slots* (layers) map
					    (let ((layer-index (find-map-index layers #'layer-id (cadr params))))
					      (setf chosenlayer layer-index)))))))))
				    
(defmessage "MAP-INFO" (message client-socket params)
  (let ((id (car params)))
    (format (socket-stream client-socket) "~a"
	    (->> *document*
		 root-maps
		 (remove-if-not (lambda (map)
				  (equalp (prin1-to-string (map-id map)) id)))
		 first
		 obj->alist
		 clean-map
		 prin1-to-string))))

				    ;; ("GET-PROPS" (lambda (message client-socket params)
				    ;; 		   (let* ((result (class-props-str (fset:lookup type->class-map (car params)))))
				    ;; 		     (format t "result: ~a~%" (prin1-to-string result))
				    ;; 		     (format (socket-stream client-socket) "~a" (prin1-to-string result)))))
				    ;; ("SET-PROP" (lambda (message client-socket params)
				    ;; 		  (let ((object-type (car params)))
				    ;; 		    (if (equalp object-type "LAYER")
				    ;; 			(destructuring-bind (object-type map-id layer-id name value) params
				    ;; 			  (let* ((object (get-layer-obj *document* (parse-integer map-id) (parse-integer layer-id)))
				    ;; 				 (slot (->> object
				    ;; 					    class-of
				    ;; 					    sb-mop:class-slots
				    ;; 					    (remove-if-not (lambda (slot-val)
				    ;; 							     (equalp (symbol-name (sb-mop:slot-definition-name slot-val)) name)))
				    ;; 					    car)))
				    ;; 			    (setf (sb-mop:slot-value-using-class (class-of object) object slot) value)))
				    ;; 			(destructuring-bind (object-type object-id name value) params
				    ;; 			  (let* ((object (get-obj *document* object-type (parse-integer object-id)))
				    ;; 				 (slot (->> object
				    ;; 					    class-of
				    ;; 					    sb-mop:class-slots
				    ;; 					    (remove-if-not (lambda (slot-val)
				    ;; 							     (equalp (symbol-name (sb-mop:slot-definition-name slot-val)) name)))
				    ;; 					    car)))
				    ;; 			    (setf (sb-mop:slot-value-using-class (class-of object) object slot) value)))))))
				    ;; ("REVERSE-BOOL-PROP" (lambda (message client-socket params)
				    ;; 			   (let ((object-type (car params)))
				    ;; 			     (if (equalp object-type "LAYER")
				    ;; 				 (destructuring-bind (object-type map-id layer-id name) params
				    ;; 				   (let* ((object (get-layer-obj *document* (parse-integer map-id) (parse-integer layer-id)))
				    ;; 					  (slot (->> object
				    ;; 						     class-of
				    ;; 						     sb-mop:class-slots
				    ;; 						     (remove-if-not (lambda (slot-val)
				    ;; 								      (equalp (symbol-name (sb-mop:slot-definition-name slot-val)) name)))
				    ;; 						     car))
				    ;; 					  (old-val (sb-mop:slot-value-using-class (class-of object) object slot)))
				    ;; 				     (setf (sb-mop:slot-value-using-class (class-of object) object slot) (not old-val))))
				    ;; 				 (destructuring-bind (object-type object-id name) params
				    ;; 				   (let* ((object (get-obj *document* object-type (parse-integer object-id)))
				    ;; 					  (slot (->> object
				    ;; 						     class-of
				    ;; 						     sb-mop:class-slots
				    ;; 						     (remove-if-not (lambda (slot-val)
				    ;; 								      (equalp (symbol-name (sb-mop:slot-definition-name slot-val)) name)))
				    ;; 						     car))
				    ;; 					  (old-val (sb-mop:slot-value-using-class (class-of object) object slot)))
				    ;; 				     (setf (sb-mop:slot-value-using-class (class-of object) object slot) (not old-val))))))))
(defmessage "LOAD-SPRITE" (message client-socket params)
  (destructuring-bind (path sprite-name) params
    (schedule-once (lambda ()
		     (setf *document*
		       (with-slots* (qmapper.root:maps qmapper.root:chosenmap qmapper.root:chosenlayer) *document*
			 (let* ((chosen-map (nth chosenmap maps)))
			   (setf chosen-map
			     (with-slots* (sprites) chosen-map
					  (let ((sprite (-> (make-sprite  :name sprite-name)
							    (init-sprite :sprite-path path :renderer *renderer*))))
					    (push
					     sprite
					     sprites))))
			 (setf (nth chosenmap maps) chosen-map))))))
    (format (socket-stream client-socket) "Scheduled sprite loading~%")))

(defmessage "LOAD-ANIMATION" (message client-socket params)
  (destructuring-bind (path framecount name) params
    (schedule-once (lambda ()
		     (let ((chosen-map (root-get-chosen-map *document*)))
		       (setf chosen-map
			     (with-slots* (qmapper.map:animatedsprites) chosen-map
					  (push
					   (-> (make-animatedsprite)
					       (init-animated-sprite
						:path path
						:framecount (parse-integer framecount)
						:renderer *renderer*))
					   animatedsprites))))))))



(defun process-editor-events (client-socket socket-stream message)
  (let* ((split-msg (split ";" message))
	 (message (car split-msg))
	 (params (cdr split-msg)))
    (if-let (event (lookup *message->event* message))
      (funcall event message client-socket params)
      (format t "Didn't find an event corresponding to ~a~%" message))
    (format (socket-stream client-socket) "~%")))

(defvar *socket* nil)

(defun-export! run-editor-server (port)
  "Run TCP server in a loop, listening to incoming connections.
  This is single-threaded version. Do NOT call this in REPL, there's a bordeaux'd wrapper under this"
  (format t "port in run-tcp-server: ~a~%" port)
  (let ((host "127.0.0.1"))
    (with-server-socket (master-socket (usocket:socket-listen host port :backlog 256))
      (unwind-protect
	   (progn
	     (setf *socket* master-socket)
	     (format t "let's wait-for-input~%")
	     (while *server-running?*
	       (if-let (socket (socket-accept master-socket :element-type 'character))
		 (progn (handler-case 
			    (progn
			      (format t "client accepted~%")
			      (process-client-socket socket #'process-editor-events))
			  (error (c)
			    (format t "ERROR ~a~%" c)))
			(usocket:socket-close socket)))))
	(progn
	  (usocket:socket-close master-socket))))))
	
(defun-export! run-editor-server-threaded (port)
  (make-thread (lambda ()
		 (run-editor-server port)) :name "doc-server-thread"))

(run-editor-server-threaded 3003)
