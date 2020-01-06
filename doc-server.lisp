(defpackage :qmapper.doc-server
  (:use :common-lisp
	:qmapper.std
	:ppcre
	:qmapper.script
	:qmapper.root 
	:cl-arrows
	:qmapper.export
	:usocket
	:bordeaux-threads))

(in-package :qmapper.doc-server)

(defparameter *server-running?* t)

;; pohja lainattu https://gist.github.com/traut/6bf71d0da54493e6f22eb3d00671f2a9

(defun send-text-to-socket (text socket)
  (let ((socket-stream (usocket:socket-stream socket)))
    (format
      socket-stream
      (format nil "~a~%" text))  ; adding a line break at the end for prettiness
    (force-output socket-stream)))

(defun logger (text &rest args)
  "Simple wrapper around format func to simplify logging"
  (apply 'format (append (list t (concatenate 'string text "~%")) args)))


(defun close-socket (socket)
  "Close a socket without raising an error if something goes wrong"
  (handler-case
      (usocket:socket-close socket)
    (error (e)
      (logger "ignoring the error that happened while trying to close socket: ~a" e)))
  (logger "socket closed"))


(defun run-tcp-server (port)
  "Run TCP server in a loop, listening to incoming connections.
  This is single-threaded version. Do NOT call this in REPL, in qmapper this'll
  be wrapped inside a QThread"
  (format t "port in run-tcp-server: ~a~%" port)
  (let ((host "127.0.0.1"))
    (with-server-socket (master-socket (usocket:socket-listen host port :backlog 256))
      (format t "let's wait-for-input~%")
      (while *server-running?*
	(if-let (socket (socket-accept master-socket :element-type 'character))
          (progn
	    (format t "client accepted~%")
	    (process-client-socket socket)
	    (socket-close socket)))))))

(defun-export! run-tcp-server-threaded (port)
  (make-thread (lambda ()
		 (run-tcp-server port)) :name "doc-server-thread"))
;; protocol parser

(defun read-stream-times (stream n)
  (labels ((do-read (n)
	     (if (pos? n)
		 (progn (format t "~a times to read ~%" n)
			(cons (read-line stream) (do-read (dec n))))
		 nil)))
    (cl-strings:join (remove-if-not (lambda (x)
				      x)
				    (do-read n)) :separator (format nil "~%"))))

(defun eval-protocol-row (socket-stream row)
  (format t "evalling row ~a~%" row)
  (let ((split-row (cl-strings:split row ":")))
    (when (pos? (length split-row))
      (let ((protocol (car split-row)))
	(values protocol
		(condp string= protocol
		       "NS" (let* ((ns (-> (cadr split-row)
					   cl-strings:clean
					   (cl-strings:replace-all "" "")))
				   (_ (format t "finding ns ~a~%" ns))
				   (script (root-findns *document* ns)))
			      (if script
				  (let* ((contents (get-prop script "CONTENTS"))
					 (_ (format t "script is ~a~%" script))
					 (firstLine (car (cl-strings:split contents (format nil "~%"))))
					 (is-lisp? (script-is-lisp? script))
					 (modeline-scanner (create-scanner "mode: (lisp|glsl)"))
					 (contains-modeline? (scan modeline-scanner contents)))
				    (if contains-modeline?
					contents
					(if is-lisp?
					    (format nil "; -*- mode: lisp; -*-~%~a" contents)
					    (format nil "; -*- mode: glsl; -*-~%~a" contents))))
				  (format t "didn't find a script~%")))
		       "SAVE-NS" (let* ((ns (nth 1 split-row))
					(rows-to-read (parse-integer (nth 2 split-row)))
					(contents (read-stream-times socket-stream rows-to-read)))
				   ;; (format t "saving contents ~a~%" contents)
				   ;; (format t "row is ~a~%" row)
				   ;; (format t "and start to remove is ~a:~a:~%"
				   ;; 	   (car split-row)
				   ;; 	   (cadr split-row))
				   (assert contents)
				   (root-savens *document* ns contents))))))))

(defun process-client-socket (client-socket)
  "Process client socket that got some activity"
  (format t "processing client-socket ~%")
  (let* ((socket-stream (usocket:socket-stream client-socket))
	 (message (read-line socket-stream)))
    (logger "got a message: ~a" message)
    (multiple-value-bind (protocol result) (eval-protocol-row socket-stream message)
      (format t "procol is ~a~%" protocol)
      (if (string= protocol "NS")
	  (format (socket-stream client-socket) "~a" result)
	  (when (string= protocol "SAVE-NS")
	    (set-doc result)
	    (format t "doc set!"))))))
