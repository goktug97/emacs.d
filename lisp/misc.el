(setq ring-bell-function 'ignore)

(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups"))
      backup-by-copying      t
      version-control        t
      delete-old-versions    t
      kept-new-versions      20
      kept-old-versions      5)

(setq custom-file (concat user-init-dir "custom.el"))
(setq browse-url-generic-program (executable-find "firefox"))

(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C--" . er/contract-region)))

(use-package undo-fu
  :ensure t
  :bind (:map evil-normal-state-map
              ("u" . 'undo-fu-only-undo)
              ("C-r" . 'undo-fu-only-redo)))

(use-package which-key
  :ensure t
  :init (which-key-mode)
  :config
  (setq which-key-allow-evil-operators t
        which-key-show-operator-state-maps t))
