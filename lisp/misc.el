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
