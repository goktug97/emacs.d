(defun remote-connect ()
  (interactive)
  (let* ((user-name (read-string "User: "))
	 (ip-address (read-string "IP: "))
	 (dir (format "/home/%s" user-name)))
    (dired (format "/ssh:%s@%s:%s" user-name ip-address dir))))

(defun load-init ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

(global-set-key (kbd "C-c D")  #'delete-file-and-buffer)


