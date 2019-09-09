(use-package pdf-tools
  :ensure t
  :config
  )

(use-package org-pdfview
  :ensure t
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; https://github.com/gjkerns/ob-julia/blob/master/ob-julia-doc.org
(load "/home/goktug/libraries/ob-julia/ob-julia.el")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (lisp .t)
   (lua . t)
   (julia . t)))

(setq org-startup-with-inline-images t)

(setq org-src-fontify-natively t
      org-src-preserve-indentation t
      org-confirm-babel-evaluate nil)

(setq org-return-follows-link t)

(setq org-file-apps
      (append '(
		("\\.pdf\\'" . "zathura %s"))
	      org-file-apps ))

(defun org-screenshot ()
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat (file-name-nondirectory (buffer-file-name))
                  "_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
      (call-process "maim" nil nil nil "-s" filename)
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]")))
  (org-redisplay-inline-images))
