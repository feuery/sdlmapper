(defun qmapper-entry (row)
  (list (random)
	(apply #'vector (mapcar #'prin1-to-string row))))

(define-derived-mode qmapper-visual-mode tabulated-list-mode "Qmapper data"
  "Major mode for browsing a list of data returned from a running qmapper instance")

(defun qmapper-visualise-data (data)
  (let* ((buffer-name (concatenate 'string "qmapper-" (prin1-to-string (random))))
	(buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer-name

      (qmapper-visual-mode)
      (setq tabulated-list-format (apply #'vector (make-list (length (car data))
							     (list "Dada 2" 9 t))))
      (setq tabulated-list-entries (mapcar #'qmapper-entry data))
      (tabulated-list-print)
      (switch-to-buffer buffer-name))))



(provide 'qmapper-visualisation)
