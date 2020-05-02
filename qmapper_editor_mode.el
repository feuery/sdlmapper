;; -*- lexical-binding: t -*-

(load-file "./qmapper-visualisation.el")

(defvar qmapper-server "localhost"
  "QMapper server's location/IP")
(defvar qmapper-port nil
  "QMapper server's port")
(defvar qmapper-ns ""
  "NS current qmapper-buffer is visiting")

(defvar qmapper-editor-port 3003
  "QMapper's editor server's port")


(defun qmapper-connect-editor (server)
  "This function sets the ip where you're running qmapper"
  (interactive "sQMapper server path: ")
  (setq qmapper-server server))

(defvar *qmapper-output-buffer* "qmapper output buffer")

(defun qmapper-send-hello-world (hw)
  (interactive "sMessage: ")
  (let* ((proc (open-network-stream *qmapper-output-buffer* (get-buffer-create *qmapper-output-buffer*)
				    qmapper-server qmapper-editor-port
				    :nowait nil)))
    (process-send-string proc (concat hw "\n"))
    (set-process-sentinel proc (lambda (p e)
				 (with-current-buffer (get-buffer-create *qmapper-output-buffer*)
				   (switch-to-buffer (get-buffer-create *qmapper-output-buffer*))
				   (goto-char 0))))))

(defun qmapper-tileset ()
  (interactive)
  (let ((proc (open-network-stream *qmapper-output-buffer* (get-buffer-create *qmapper-output-buffer*)
				    qmapper-server qmapper-editor-port
				    :nowait nil)))
    (process-send-string proc (concat "SHOW-TILESET\n"))))

(defun qmapper-map ()
  (interactive)
  (let ((proc (open-network-stream *qmapper-output-buffer* (get-buffer-create *qmapper-output-buffer*)
				    qmapper-server qmapper-editor-port
				    :nowait nil)))
    (process-send-string proc (concat "SHOW-MAP\n"))))

(defun qmapper-load-tileset (tileset-path tileset-name)
  (interactive "fTileset's path \nsTileset's name: ")
  (let ((proc (open-network-stream *qmapper-output-buffer* (get-buffer-create *qmapper-output-buffer*)
				    qmapper-server qmapper-editor-port
				    :nowait nil))
	(tileset-path (file-truename tileset-path)))
    (process-send-string proc (concat "LOAD-TILESET;" tileset-path ";" tileset-name"\n"))))


(defun query-qmapper (cmd callback)
  (let* ((buffer-name (concatenate 'string "qmapper-" (prin1-to-string (random))))
	 (proc (open-network-stream  buffer-name (get-buffer-create buffer-name)
 				    qmapper-server qmapper-editor-port
 				    :nowait nil)))
      (process-send-string proc cmd)
      (set-process-sentinel proc (lambda (p e)
				   (with-current-buffer (get-buffer buffer-name)
				     (funcall callback (buffer-string)))
				   (kill-buffer buffer-name)))
      nil))

(require 'cl-lib)
(cl-defun query-lambda (&key selection-command lambdas-to-run-inside-buffer)
  (lambda (dada)
    (let ((id-name-pairs (car (read-from-string dada))))
      (qmapper-visualise-data id-name-pairs (if selection-command
						(lambda (id)
						  (query-qmapper (concat selection-command ";" id ";\n") (lambda (&rest aaa)
													   aaa)))
					      (lambda (id)
						nil))
			      lambdas-to-run-inside-buffer))))

(defun qmapper-list-tilesets ()
  (interactive)
  (query-qmapper "LIST-TILESETS;\n"
		 (query-lambda :selection-command "SELECT-TILESET")))

(defun qmapper-new-map (w h)
  (interactive "nMap width: \nnMap height: ")
  (query-qmapper (concat "CREATE-MAP;" (prin1-to-string w) ";" (prin1-to-string h)";\n") (lambda (&rest aaa) nil)))

(defun qmapper-list-maps ()
  (interactive)
  (query-qmapper "LIST-MAPS;\n"
		 (query-lambda :selection-command "SELECT-MAP"
			       :lambdas-to-run-inside-buffer (list (lambda ()
								     (local-set-key "c" (lambda ()
											  (interactive)
											  (call-interactively #'qmapper-new-map)
											  (kill-this-buffer)
											  (qmapper-list-maps))))))))

(defun qmapper-list-tools ()
  (interactive)
  (query-qmapper "LIST-TOOLS;\n"
		 (query-lambda :selection-command "SELECT-TOOL")))

(defun qmapper-list-layers ()
  (interactive)
  (query-qmapper "LIST-LAYERS;\n" (query-lambda :lambdas-to-run-inside-buffer (list (lambda ()
										      (local-set-key "c" (lambda ()
													   (interactive)
													   (query-qmapper "CREATE-LAYER;\n" (lambda (result)
																	      (qmapper-list-layers))))))))))

(defun qmapper-fetch-ns (server port ns)
  (let ((buffer-name (concat "QMAPPER: " ns)))
    (if (get-buffer buffer-name)
	(kill-buffer buffer-name))
    
    (let ((proc (open-network-stream buffer-name (get-buffer-create buffer-name)
				     server port
				     :nowait nil)))
      ;; (message (concat "Process coding system is " (prin1-to-string (process-coding-system proc))))
      (process-send-string proc (concat "NS:" ns "\n"))
      (set-process-sentinel proc (lambda (p e)
				   (with-current-buffer (get-buffer-create buffer-name)
				     (switch-to-buffer (get-buffer-create buffer-name))

				     (goto-char 0)

				     (cond
				      ((save-excursion
					 (search-forward "-*- mode: lisp; -*-" nil t))
				       (lisp-mode))
				      ((save-excursion
					 (search-forward "-*- mode: glsl" nil t))
				       (glsl-mode)))
				     
				     (qmapper-editor-mode)
				     (rename-buffer buffer-name)

				     
				     (setq qmapper-server server)
				     (setq qmapper-port port)
				     (setq qmapper-ns ns)))))))

(defun qmapper-save-ns ()
  (interactive)
  (let* ((server qmapper-server)
	 (port qmapper-port)
	 (ns qmapper-ns)
	 (buffer-name (concat "QMAPPER: " ns)))
    (if (not (equal ns ""))
	(with-current-buffer buffer-name
	  (let* ((buf-content (buffer-string))
		 (n-of-rows (length (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
		 (proc (open-network-stream "*qmapper-tmp*" (get-buffer-create "*qmapper-tmp*")
					    server port
					    :nowait nil)))
	    (process-send-string proc (concat "SAVE-NS:" ns ":" (int-to-string n-of-rows) "\n" buf-content "\n"))
	    (set-process-sentinel proc (lambda (p e)
					 (set-buffer-modified-p nil)
					 (message (concat "Saved " ns " to " server ":" (int-to-string port)))))))
      (save-buffer))))

(defun qmapper-find-file (find-file &rest args)
  (message (concat "Searching " (prin1-to-string (car args))))
  (let* ((path (car args)))
      (cond
       ((string-match "^QM:\\(\\w+\\):\\([0-9]+\\):\\(.+\\)" path)
	(let ((server (match-string 1 path))
	      (port (string-to-number (match-string 2 path)))
	      (ns (match-string 3 path)))
	  (qmapper-fetch-ns server port ns)))
       (t (apply find-file args)))))
  

(defun to-kill-buffer ()
  (interactive)
  (if (or (not (buffer-modified-p))
  	       (and (buffer-modified-p)
  		    (y-or-n-p (concat "Buffer " (prin1-to-string (current-buffer)) " modified. Kill anyway?"))))
      (kill-buffer (current-buffer))))

(define-minor-mode qmapper-editor-mode
  "QMapper doc-server client"
  :lighter " qmapper"
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-x C-s") #'qmapper-save-ns)
	    (define-key map (kbd "C-x k") 'to-kill-buffer)
	    (advice-add 'find-file :around #'qmapper-find-file)
						   
	    map)  
  :global nil)
				     
