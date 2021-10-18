;; Minimize garbage collection during startup
(setq gc-cons-threshold most-positive-fixnum)

;; Lower threshold back to 8 MiB (default is 800kB)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (expt 2 23))))

(defconst user-init-dir
  (cond ((boundp 'user-emacs-directory)
	 user-emacs-directory)
	((boundp 'user-init-directory)
	 user-init-directory)
	(t "~/.emacs.d/")))

(setq custom-file (concat user-init-dir "custom.el"))

(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups"))
      backup-by-copying      t
      version-control        t
      delete-old-versions    t
      kept-new-versions      20
      kept-old-versions      5)

;; Some optimizations from doom-emacs
;; https://github.com/hlissner/doom-emacs/
(unless (daemonp)
  (advice-add #'display-startup-echo-area-message :override #'ignore))

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(setq auto-mode-case-fold nil)

(setq frame-inhibit-implied-resize t)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)

(setq bidi-inhibit-bpa t) 
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq redisplay-skip-fontification-on-input t)

(unless (eq system-type 'darwin) (setq command-line-ns-option-alist nil))
(unless (eq system-type 'gnu/linux) (setq command-line-x-option-alist nil))

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(use-package straight
             :custom (straight-use-package-by-default t))

(setq package-enable-at-startup nil
      package-user-dir (concat user-init-dir "elpa/")
      package-gnupghome-dir (expand-file-name "gpg" package-user-dir)
      package-archives
        (list (cons "gnu"   "http://elpa.gnu.org/packages/")
              (cons "melpa" "http://melpa.org/packages/")
              (cons "org"   "http://orgmode.org/elpa/")))

(advice-add #'package--ensure-init-file :override #'ignore)

;; all-the-icons for the dashboard
;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :defer t
  :ensure t)

(use-package dashboard
  :init
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  ;; (setq dashboard-projects-backend `projectile)
  ;; (setq dashboard-set-heading-icons t)
  ;; (setq dashboard-set-file-icons t)
  ;; (setq dashboard-set-navigator t)
  (setq dashboard-set-init-info t)
  (setq dashboard-items '(;; (recents  . 5)
                          ;; (bookmarks . 5)
                          ;; (projects . 5)
                          ;; (registers . 5)
                          )))

