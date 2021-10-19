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

(menu-bar-mode -1) 
(toggle-scroll-bar -1) 
(tool-bar-mode -1) 
(delete-selection-mode 1)
(show-paren-mode t) 
(fset `yes-or-no-p `y-or-n-p)
(setq-default indent-tabs-mode nil)
(setq apropos-sort-by-scores t)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist
	     '(font . "-*-Hack-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
