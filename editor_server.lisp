(defpackage :qmapper.editor-server
  (:use :common-lisp
	:qmapper.app-state
        :qmapper.std
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

(defparameter *message->event* (map ("SHOW-TILESET" (lambda (message client-socket params)
						      (format (socket-stream client-socket) "Hello :D ~%")))
				    ("LIST-TILESETS" (lambda (message client-socket params)
						       (format (socket-stream client-socket)
							       (with-slots (tilesets) *document*
								 (->> (zipmap 
								       (mapcar #'tileset-id tilesets)
								       (mapcar #'tileset-name tilesets))
								      (format nil "(~{~s~^~%~})"))))))
				    ("SELECT-TILESET" (lambda (message client-socket params)
							(let* ((searched-id (parse-integer (car params)))
							       (index (->> (range (length (root-tilesets *document*)))
									   (mapcar #'dec)
									   (remove-if-not (lambda (index)
											    (with-slots (qmapper.tileset:id) (nth index (root-tilesets *document*))
											      (equalp searched-id qmapper.tileset:id)))))))
							  (if index
							      (setf (root-chosentileset *document*) (car index))))))
				    
				    
				    
				    ("LOAD-TILESET" (lambda (message client-socket params)
						      (let* ((tileset-path (car params))
							     (tileset-name (cadr params)))
							(schedule-once (lambda ()
									 (with-slots (tilesets) *document*
									   (push
									    (make-instance 'qmapper.tileset:tileset :name tileset-name :tileset-path tileset-path :renderer *renderer*)
									    tilesets)))))))))

;;(root-chosentileset *document*)

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
		 (unwind-protect 
		      (progn
			(format t "client accepted~%")
			(process-client-socket socket #'process-editor-events))
		   (socket-close socket)))))
	(progn
	  (usocket:socket-close connection)
	  (usocket:socket-close socket))))))
	

(defun-export! run-editor-server-threaded (port)
  (make-thread (lambda ()
		 (run-editor-server port)) :name "doc-server-thread"))

(run-editor-server-threaded 3003)
