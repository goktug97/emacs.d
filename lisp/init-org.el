(use-package org
  :ensure t
  :config
  (require 'org-tempo)
  (setq org-startup-with-inline-images t
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-confirm-babel-evaluate nil
        org-return-follows-link t
        org-startup-truncated nil)
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (lisp .t)
     (shell . t))))

(use-package pdf-tools
  :ensure t
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (pdf-tools-install))

(use-package org-pdfview
  :ensure t
  :after org
  :config 
  (add-to-list
   'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link)))))

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun org-screenshot ()
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (sleep-for 0 500)
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
      (call-process "maim" nil nil nil "-s" filename)
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]")))
  (org-redisplay-inline-images))

(defun org-replace-link-by-link-description ()
  "Replace an org link by its description or if empty its address"
  (interactive)
  (if (org-in-regexp org-bracket-link-regexp 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description (if (match-end 3) 
                               (org-match-string-no-properties 3)
                             (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))
