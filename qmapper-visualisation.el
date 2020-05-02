;; -*- lexical-binding: t -*-

(defun qmapper-entry (row)
  (list (random)
	(apply #'vector (mapcar #'prin1-to-string row))))

(defun qmapper-selected-row-id ()
  (aref 
   (cadr
    (assoc (tabulated-list-get-id) tabulated-list-entries))
   0))


(defun qmapper-visual-mode-map (ret-fn)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") (lambda ()
				  (ret-fn (qmapper-selected-row-id))))
    map))

(define-derived-mode qmapper-visual-mode tabulated-list-mode "Qmapper data"
  "Major mode for browsing a list of data returned from a running qmapper instance")

(defun qmapper-visualise-data (data ret-lambda &optional lambdas-to-run-inside-buffer)
  (let* ((buffer-name (concatenate 'string "qmapper-" (prin1-to-string (random))))
	(buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer-name
      (add-hook 'qmapper-visual-mode-hook (lambda ()
					    (local-set-key (kbd "<return>") (lambda ()
									      (interactive)
									      (let ((id (qmapper-selected-row-id)))
										(message (concat "selected row id " id))
										(funcall ret-lambda id))))
					    (setq qmapper-visual-mode-hook (cdr qmapper-visual-mode-hook))))
      (qmapper-visual-mode )
      (setq tabulated-list-format (apply #'vector (make-list (length (car data))
							     (list "Dada 2" 9 t))))
      (setq tabulated-list-entries (mapcar #'qmapper-entry data))
      (tabulated-list-print)
      (switch-to-buffer buffer-name)
      (dolist (l lambdas-to-run-inside-buffer)
	(funcall l)))))



(provide 'qmapper-visualisation)
