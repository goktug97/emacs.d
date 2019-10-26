(require 'package)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp/" user-emacs-directory))

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(package-initialize)

(setq ring-bell-function 'ignore)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
	 user-emacs-directory)
	((boundp 'user-init-directory)
	 user-init-directory)
	(t "~/.emacs.d/")))

(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups"))
      backup-by-copying      t 
      version-control        t
      delete-old-versions    t  
      kept-new-versions      20 
      kept-old-versions      5) 

(defun load-user-file (file)
  (interactive "f")
  "Load a file in current user's configuration directory"
  (load-file (expand-file-name file user-init-dir)))

;; (load-user-file "./lisp/multiple-cursors.el")
(load-user-file "./lisp/evil.el")
(load-user-file "./lisp/font.el")
(load-user-file "./lisp/apperance.el")
(load-user-file "./lisp/paredit.el")
(load-user-file "./lisp/slime.el")
(load-user-file "./lisp/julia.el")
(load-user-file "./lisp/init-python.el")
(load-user-file "./lisp/helm.el")
(load-user-file "./lisp/magit.el")
(load-user-file "./lisp/company.el")
(load-user-file "./lisp/functions.el")
(load-user-file "./lisp/markdown.el")
(load-user-file "./lisp/init-org.el")
(load-user-file "./lisp/spotify.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-mc zones lua-mode multiple-cursors simple-httpd wgrep-helm w3m use-package paredit org-pdfview org-link-minor-mode org-bullets multi markdown-mode magit-popup htmlize helm-slime helm-descbinds helm-company haskell-mode exwm evil-visual-mark-mode evil-magit evil-commentary elpy doom-themes clang-format))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )