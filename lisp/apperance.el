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

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (eval-after-load "org"
    (doom-themes-org-config))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (with-selected-frame frame
                    (load-theme 'doom-molokai t))))
    (load-theme 'doom-molokai t)))

(use-package fic-mode
   :straight nil
   :config
   (add-to-list 'fic-highlighted-words "NOTE")
      (set-face-attribute 'font-lock-fic-face nil :weight 'bold)
   (add-hook 'prog-mode-hook 'turn-on-fic-mode)
   :init 
    (setq 
      fic-background-color nil
      fic-foreground-color "Orange"))

