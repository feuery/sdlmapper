(defpackage :qmapper.root
  (:use :common-lisp
	:cl-arrows
	:qmapper.layer
	:rutils.abbr
	:qmapper.std
	:qmapper.export
	:qmapper.script)
  (:shadowing-import-from :cl-strings :replace-all)
  (:export :root-layers :root-chosentool :root-get-chosen-map :root-chosenTile :chosentile :root-maps :maps :root-chosenLayer :chosenlayer :chosentile :tilesets  :root-chosentileset :root-chosenMap :chosentileset :root-tilesets :*document* :chosentool :chosenmap))


(in-package :qmapper.root)

(defun-export! script-ns->id (root ns)
  (clean-key 
   (get-prop (->> root
		  root-scripts 
		  (convert 'list)
		  (mapcar #'cdr)
		  (filter (lambda (s)
			    (format t "s is ~s" s)
			    (string= (script-ns s)
				     ns)))
		  car) "ID")))

(defun-export! script-id->ns (root id)
  ;; (clean-key 
   (script-ns (get-prop (->> root
		  root-scripts )
	     id)))

(defun-export! get-all-scripts (root)
  (->> root
       root-scripts
       (convert 'list)
       (mapcar #'cdr)))    

(defclass* root
    (layers nil)
  (maps nil (lambda (new-maps)
	      (not (null new-maps))))
  (scripts nil)
  (tiles nil)
  (tilesets nil)
  (chosenMap 0)
  (chosenLayer 0)
  (chosenTileset  0)
  (chosenTile  nil)
  (StdVertexShader  "defaultVertex")
  (StdFragmentShader  "defaultFragment")
  (StdTileviewFragShader  "default.tileView")
  ;; (animatedSprites :accessor root-animatedSprites :initarg :animatedSprites :initform  nil)
  ;; (sprites :accessor root-sprites :initarg :sprites :initform   nil)
  (selected-coordinates  (list 0 0 0 0))
  (chosentool :pen))

(defun root-get-chosen-map (*this*)
  (let ((id (root-chosenmap *this*))
	(maps (root-maps *this*)))
    (get-prop maps id)))

(defun root-contents-StdFragment (*this*)
  (let* ((ns (root-stdfragmentshader *this*))
	 (result (->> (root-scripts *this*)
		      (fset:filter (lambda (b script)
				     (equalp (script-ns script) ns)))
		      (convert 'list)
		      cdar)))
    (script-contents result)))

(defun root-contents-StdVertex (*this*)
  (let* ((ns (root-stdvertexshader *this*))
	 (result (->> (root-scripts *this*)
		      (fset:filter (lambda (b script)
				     (equalp (script-ns script) ns)))
		      (convert 'list)
		      cdar)))
    (script-contents result)))
(defun root-contents-stdTileviewFragShader (*this*)				      
  (let* ((ns (root-stdtileviewfragshader *this*))
	 (result (->> (root-scripts *this*)
		      (fset:filter (lambda (b script)
				     (equalp (script-ns script) ns)))
		      (convert 'list)
		      cdar)))
    (script-contents result)))
(defun root-registrySize (*this*)
  (reduce #'+ (mapcar (lambda (col)
			(length (convert 'list col)))
		      (list
		       (root-layers *this*)
		       (root-maps *this*)
		       (root-Scripts *this*)
		       (root-tiles *this*)
		       (root-tilesets *this*)))))
(defun root-fetchRegister (*this* Type id)
  (plist-get 
   (condp equalp Type
	  ;; TODO write a thing that catenates sprites from every map
	  ;; "AnimatedSprite" (root-animatedSprites *this*)
	  "Layer" (root-layers *this*)
	  "Map" (root-maps *this*)
	  "Script" (root-Scripts *this*)
	  ;; "Sprite" (root-sprites *this*)
	  "Tile" (root-tiles *this*)
	  "Tilesets" (root-tilesets *this*))
   id))

(defun root-findNs (*this* ns)
  (let* ((list-scripts (convert 'list (root-scripts *this*)))
	 (scripts (mapcar #'cdr list-scripts)))
    (let ((result (car (filter (lambda (script)
				 (equalp (Script-ns script) ns)) scripts))))
      result)))
(defun root-saveNs (*this* ns content)
  (let* ((scripts (root-scripts *this*))
	 (id (script-ns->id *this* ns))
	 (new-script (-> (get-prop scripts id)
			 (set-Script-Contents! content)))
	 (scripts (set-prop scripts id new-script)))
    (set-root-scripts! *this* scripts)))
(defun root-containsNs (*this* ns)
  (not (filter (lambda (script)
		 (equalp (script-ns script) ns)) scripts)))
(defun root-registryOf (*this* type)
  (let ((result 
	 (cond ;; ((equalp type "AnimatedSprite")
	   ;;  (root-animatedSprites *this*))
	   ((equalp type "Layer")
	    (root-layers *this*))
	   ((equalp type "Map") (root-maps *this*))
	   ((equalp type "Script") (root-scripts *this*))
	   ;; ((equalp type "Sprite") (root-sprites *this*))
	   ((equalp type "Tile") (root-tiles *this*))
	   ((equalp type "Tileset") (root-tilesets *this*))
	   (t
	    (scm-puts "registryOf got unrecognized type")
	    (scm-puts type)
	    nil))))
    (if result
	(drop-plist-keys result)
	result)))
(defun root-registryToList (*this*)
  (let* ((s (->> (concatenate 'list
			      (convert 'list (root-animatedSprites *this*))
			      (convert 'list (root-layers *this*))
			      (convert 'list (root-maps *this*))
			      (convert 'list (root-scripts *this*))
			      (convert 'list (root-sprites *this*))
			      (convert 'list (root-tiles *this*))
			      (convert 'list (root-tilesets *this*)))
		 (mapcar (lambda (l)
			   (if (symbolp (car l))
			       (cons (replace-all (symbol-name (car l)) "\"" "")
				     (cdr l))
			       (cons (replace-all (car l) "\"" "")
				     (cdr l)))))))
	 (alist (->>  s
		      seq
		      (reduce #'concat)))
	 (concd (alist->map alist)))
    concd))

(defun-export! get-selected-tileset (root)
  (let ((selected-ind (root-chosenTileset root)))
    (format t "selected tileset is ~a~%" selected-ind)
    (cdr (nth selected-ind (convert 'list (root-tilesets root))))))

(defun-export! init-root! ()
  (make-root))

(defun-export! select-tile (root tilex tiley)
  (assert root)
  (let* ((selected-coords (root-selected-coordinates root))
	 (_ (format t "selected-coords: ~a~%" selected-coords))
	 (left (fset:lookup selected-coords 0))
	 (top (fset:lookup selected-coords 1))
	 (right (fset:lookup selected-coords 2))
	 (bottom  (fset:lookup selected-coords 3)))
    (format t "tilex tiley: ~a~%" (list tilex tiley))
    ;; (assert (pos? tilex))
    ;; (assert (pos? tiley))
    (cond ((equalp selected-coords
		   #[ 0 0 0 0 ])
	   (format t "SELECTED COORDS nollattu listaan ~a~%" (list tilex tiley tilex tiley))
	   (set-prop root "SELECTED-COORDINATES" (seq tilex tiley tilex tiley)))
	  ((and tilex (< tilex left))
	   (set-prop-in root (list "SELECTED-COORDINATES" 0) tilex))
	  ((and tiley (< tiley top))
	   (set-prop-in root (list "SELECTED-COORDINATES" 1) tiley))
	  ((and tilex (> tilex right))
	   (set-prop-in root (list "SELECTED-COORDINATES" 2) tilex))
	  ((and tiley (> tiley bottom ))
	   (set-prop-in root (list "SELECTED-COORDINATES" 3) tiley))
	  (t root))))

(defun-export! reset-selection (root)
  (assert root)
  (set-root-selected-coordinates! root (seq 0 0 0 0)))
	  
  

;; test code for events
;; (-> (init-root!)
;;     (set-prop "maps" 22)
;;     (add-event!  "maps" (lambda (this x)
;; 			  (format t "Sijoitettiin x-propsuun ~a~%" x)
;; 			  this))
;;     (add-event! "maps" (lambda (this m)
;; 			 (set-prop this "lolz" (* m m))))
;;     (update-prop "maps" #'inc)
;;     (update-prop "maps" #'inc)
;;     (get-prop "lolz"))

(defun-export! push-map (root m)
  (let* ((maps (set-prop (root-maps root)
			 (get-prop m "ID")
			 m))
	 (r (if (not (root-chosenMap root))
		(set-root-chosenMap! root (get-prop m "ID"))
		root))
	 (final-root (set-root-maps! root maps)))
    ;; (format t "Final root at push-map is ~a~%" final-root)
    final-root))

;; (get-prop-in *document* '(maps))
;; (assoc 'G1468 '((G1468 . 2323)))

(defun-export! find-path-of (root obj)
  (let ((type-sym (format nil "~as" (q-type-of obj))))
    (unless type-sym
      (format t "didn't recognize type ~a~%" (q-type-of obj))
      (format t "obj is ~a~%" obj))
    (let ((key (get-prop obj "ID")))
      (list type-sym (clean-key key)))))

(defun-export! delete-object (root path)
  (dissoc-prop-in root path))
		    

(defun-export! find-by-id (root id)
;  (declaim (optimize (debug 3)))
  (let* ((result-set (->> root
			  root-registrytolist
			  (convert 'list)
			  (mapcar #'cdr)
			  (remove-if-not (lambda (r)
					   (let* ((row-id (get-prop r "ID"))
						  (row-id (replace-all (if (symbolp row-id)
									   (symbol-name row-id)
									   row-id)
								       "\"" ""))				    
						  (result (string= row-id id)))
					     (format t "(string= ~a ~a) => ~a~%" (prin1-to-string row-id) (prin1-to-string id) result)
					     result)))))
	 (len (length result-set)))
    ;; (format t "result-set: ~a members~%" len)
    ;; (format t "find-by-id found ~a elements~%" len)
    (when (> len 0)
	(when (> len 1)
	  (format t "find-by-id found ~a elements~%" len))
	(car result-set))))

(defun-export! push-script (root scr)
  ;; (declare (optimize (debug 3)))
  (assert root)
  (let ((result 
	 (set-root-scripts! root
			    (let* ((root-scripts (root-scripts root))
				   (_ (format t "root-scripts: ~a~%" root-scripts))
				   (result (set-prop root-scripts
						 (get-prop scr "ID")
						 scr)))
			      result))))
    result))

    
(defun-export! is-ns-lisp? (root ns)
  (let ((scr (root-findNs root ns)))
    (format t "found script ~a~%" scr)
    (Script-is-lisp? scr)))

(defun-export! is-ns-glsl? (root ns)
  (let ((scr (root-findNs root ns)))
    (Script-is-glsl? scr)))

(defun-export! push-sprite2 (root mapInd sprite-id sprite)
  (-> root
      (update-prop 'sprites (lambda (sprites)
			      (format t "updating sprites~%")
			      (let ((res
				     (-> sprites
					 (set-prop sprite-id 
						   (set-prop sprite 'id (clean-key sprite-id))))))
				(format t "updated sprites~%")
				res)))
      (update-prop-in (list "maps" mapInd "sprites") (lambda (sprites)
						       (format t "Updating sprites in a map~%")
						       (let ((sprites (fset:filter (lambda (spr-id)
										     (not (equalp spr-id sprite-id)))
										   sprites)))
							 (format t "Found sprites, conssing to the start of them~%")
							 (let ((result 
								(with-first sprites (clean-key sprite-id))))
							   (format t "result ~a~%" result)
							   result))))))

(defun-export! push-animation (root mapInd animation)
  (let ((animation-id (get-prop animation "ID")))
    (-> root
	(update-prop 'animatedSprites (lambda (animatedSprites)
					(format t "updating animatedSprites~%")
					(let ((res
					       (-> animatedSprites
						   (set-prop animation-id 
							     (set-prop animation 'id animation-id)))))
					  (format t "updated animatedSprites~%")
					  res)))
	(update-prop-in (list "maps" mapInd "animatedSprites") (lambda (animatedSprites)
								 (let ((animatedSprites (fset:filter (lambda (spr-id)
												       (not (equalp spr-id animation-id)))
												     animatedSprites)))
								   (with-first animatedSprites animation-id)))))))

(defvar *document* (init-root!))
(defvar *engine-document* nil)
(export '*engine-document*)
(export '*document*)

(defvar *document-hooks* '())
(defvar *undo-stack* '())

(defun-export! set-doc (doc)
  (assert (not (functionp doc)))
  (assert doc)

  ;; (dolist (c (convert 'list doc))
  ;;   (assert (not (symbolp (car c)))))
  
  (push *document* *undo-stack*)
  (setf *undo-stack* (take *undo-stack* 20))
  (setf *document* doc)
  (dolist (l *document-hooks*)
    (if (and l
	     (functionp l))
	(progn
	 (format t "Lol koukku~a~%" (function-lambda-expression l))
	 (funcall l doc)))))

(defun-export! set-engine-doc (doc)
  (assert (not (functionp doc)))
  (assert doc)

  (setf *engine-document* doc))



(defun-export! undo! ()
  (setf *document* (car *undo-stack*))
  (setf *undo-stack* (cdr *undo-stack*)))

(defun-export! register-doc-hook (l)
  (if (and l
	   (functionp l))
      (push l *document-hooks*)
      (format t "hook ~a can't be registered~%" l)))

(defun-export! clear-engine-document! ()
  (setf *engine-document* *document*)
  *engine-document*)

(defun-export! copy-doc-to-engine! ()
  (setf *engine-document* *document*))

(defun-export! tileset-count! ()
  (length (root-tilesets *document*)))

(defun-export! load-root (data)
  (-> data
      read-from-string
      eval))

(defun-export! add-script-to-map (root map-id script-ns)
  ;; (format t "Adding script ~a to map ~a~%" script-ns map-id)
  ;; (format t "Doc: ~a~%" root)
  (update-prop-in root (list "MAPS" map-id "SCRIPTS-TO-RUN") (lambda (scripts)
							       ;; (assert scripts)
							       
							       (let ((id (script-ns->id root script-ns)))
								 (assert id)
								 (fset:with-first (or scripts (fset:empty-seq)) id)))))

(defun-export! drop-script-from-map (root map-id script-ns)
  (update-prop-in root (list "MAPS" map-id "SCRIPTS-TO-RUN") (partial #'fset:filter (lambda (script-id)
										      (not (string= (script-id->ns root script-id) script-ns))))))

;;(filter-seqs *document*)

;; (setf *document* (eval (read-from-string (prin1-to-string (filter-unserializables *document*)))))

;; (equalp (filter-seqs (eval (read-from-string (prin1-to-string (filter-unserializables *document*)))))
;; 	(filter-seqs (filter-unserializables *document*)))


(defun find-sdl-objs (m)
  (cond ((hash-table-p m)
	 (find-sdl-objs (fset:convert 'fset:map m)))
	((fset:seq? m)
	 (find-sdl-objs (fset:convert 'list m)))
	((listp m)

	 (rutil:mapcat #'find-sdl-objs m))
	((and (fset:map? m)
	      (string= (fset:lookup m "TYPE") "OBJ"))
	 (list m));)
	((fset:map? m)
	 (rutil:mapcat #'find-sdl-objs (vals m)))))

;; (save-doc! *document* #P"/home/feuer/skriptitesti.sdlmap")


(defun-export! save-doc! (doc path)
  ;; ensin puhdistetaan clean-hashmaps:illa
  ;; sitten iteroidaan läpi samalla rungolla kuin em. funktiossa, ja jokaiselta oliolta kysytään "NONSERIALIZABLE-FIELDS", eli lista avaimia jotka sitten dissocataan
  ;; kun tää on tehty, voidaan formatilla lyödä koko rakenne $path.lispiin

  (let ((output-dir-name (pathname (str "/tmp/sdlmapper-" (random 999999) "/")))
	(objs (find-sdl-objs doc)))
   (ensure-directories-exist output-dir-name)
   (format t "Temp directory ~a exists~%" output-dir-name)

   (dolist (obj objs)
      (let* ((id (qmapper.obj:obj-id obj))
	     (filename (pathname (str output-dir-name "/" id ".png"))))
	(sdl2-image:save-png (qmapper.obj:obj-surface obj)
			     (str filename))))

    (spit (filter-unserializables doc) (pathname (str output-dir-name "/doc.lisp")))
    (archive:with-open-archive (archive path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (dolist (f (cl-fad:list-directory output-dir-name)
		 (archive:finalize-archive archive))
	(let ((entry (archive:create-entry-from-pathname archive f)))
	  (archive:write-entry-to-archive archive entry))))

    (cl-fad:delete-directory-and-files output-dir-name)
    (format t "Wrote document to ~a~%" path)))

(defvar *dst-path* nil
  "A moronic dynamic scope passing hack to pass a destination path to our hacked version of archive:extract-entry")

(defun entry-name (pname)
  (str (pathname-name pname) "." (pathname-type pname)))

(defmethod archive:extract-entry ((archive archive:tar-archive) (entry archive::tar-entry))
  "A hacked up version of the original with a way to pass the destination where to extract files as an argument"
  (let ((name (str *dst-path* "/" (entry-name (pathname (archive:name entry))))))
    (cond
      ((= (archive::typeflag entry) archive::+tar-directory-file+)
       (ensure-directories-exist name))
      ((= (archive::typeflag entry) archive::+tar-regular-file+)
       (ensure-directories-exist name)
       (with-open-file (stream name :direction :output
                               :if-exists :supersede
                               :element-type '(unsigned-byte 8))
         (archive::transfer-entry-data-to-stream archive entry stream)))
      (t
       (error 'unhandled-extract-entry-error :typeflag (archive::typeflag entry))))))

(defun-export! load-doc! (path renderer)
  (let* ((input-dir-name (pathname (str "/tmp/sdlmapper-" (random 999999) "/")))
	 (*dst-path* input-dir-name))
    (format t "Files are to be extracted at ~a~%" input-dir-name)
    (ensure-directories-exist input-dir-name)
    (archive:with-open-archive (archive path :direction :input)
      (archive::extract-files-from-archive archive))

    (let ((loaded-document (->> (str input-dir-name "/doc.lisp")
				slurp
				read-from-string
				eval
				filter-seqs
				(walk-and-transform
				 ;; if a leave is a map and has a type of obj
				 (lambda (leave)
				   (let ((result 
					   (and (fset:map? leave)
						(equalp (fset:lookup leave "TYPE")
							"OBJ"))))
				     result))
				 ;; then set its surface and texture correctly
				 (lambda (leave)
				   (let ((img-path (str input-dir-name (fset:lookup leave "ID") ".png")))
				     ;;(format t "Loading image ~a~%" img-path)
				     (with-slots* (surface texture) leave
				       (setf surface (sdl2-image:load-image img-path))
				       (setf texture (sdl2:create-texture-from-surface renderer surface)))))))))
      (format t "Removing files from ~a~%" input-dir-name)
      (cl-fad:delete-directory-and-files input-dir-name)
      loaded-document)))

(defun eval-map-scripts (doc map onload-key)
  (unless (or (hash-table-p map)
	      (fset:map? map))
    (format t "Map is something rubbish ~a~%" map))
  (assert (or (hash-table-p map)
	      (fset:map? map)))
  (assert (equalp (fset:lookup map "TYPE") "MAP"))
  (assert (or (equalp onload-key :onload)
	      (equalp onload-key :on-unload)))

  (let* ((script-key (if (equalp onload-key :onload)
			 "ON-LOAD-SCRIPTS"
			 "ON-UNLOAD-SCRIPTS"))
	 (script-keys (fset:lookup map script-key)))
    (if (listp script-keys)
	(dolist (ns script-keys)
	  (let ((script (->> (root-scripts doc)
			     (remove-if-not (lambda (scr)
					      (equalp (script-ns scr) ns)))
			     first)))
	    (when (and script
		       (script-contents script))
	      (let* ((inputdir (pathname (str "/tmp/sdlmapper-" (random 999999) "/")))
		     (script-filename (str inputdir "tmp.lisp")))
		(ensure-directories-exist inputdir)
		(spit-raw (script-contents script) script-filename)
		(load script-filename)

		
		(cl-fad:delete-directory-and-files inputdir)))))
	(format t "Ei löydy skriptejä ~a mapista ~a~%" script-key (fset:convert 'fset:map map)))))
    
  

(defun-export! engine-choose-map (new-map-index &key (initial? nil))
					;(declaim (optimize (speed 0) (space 0) (debug 3)))
  (setf *engine-document* (with-slots* (chosenmap maps scripts) *engine-document*
			    ;;Maps: (Map 1 Oikeasti uusi kartta)
			    
			    (let ((old-map (nth chosenmap maps)))
			      (if new-map-index
				  (progn
				    (unless initial?
				      (eval-map-scripts *engine-document* old-map :on-unload))
				    
				    (setf chosenmap new-map-index)				    
				    (assert chosenmap)
				    (let ((new-map (nth chosenmap maps)))
				      (eval-map-scripts *engine-document* new-map :onload)))

				  (format t "Couldn't find new map index when choosing a new map"))))))
