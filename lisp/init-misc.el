(use-package evil
  :ensure t
  :init
  (setq evil-search-module 'evil-search
        evil-cross-lines t
        evil-move-beyond-eol t
        evil-move-cursor-back nil
        evil-want-fine-undo t
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-u-scroll t
        evil-want-integration t
        evil-want-keybinding nil)
  (setq-default evil-symbol-word-search t)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs)
  (setq evil-mode-line-format nil)
  (setq evil-default-modeline-color
        (cons (face-background 'mode-line)
              (or (face-foreground 'mode-line)
                  "white")))
  (add-hook 'post-command-hook 'evil-color-modeline)
  :preface
  (defun evil-color-modeline ()
    (let ((color
       (cond
        ((minibufferp) evil-default-modeline-color)
        ((evil-insert-state-p)
         '("#006fa0" . "#ffffff"))
        ((evil-emacs-state-p)
         '("#444488" . "#ffffff"))
         (t evil-default-modeline-color))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color)))))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package helm
  :defer t
  :ensure t
  :init
    (global-set-key [remap execute-extended-command] 'helm-M-x)
    (global-set-key [remap find-file] 'helm-find-files)
    (global-set-key [remap occur] 'helm-occur)
    (global-set-key [remap list-buffers] 'helm-mini)
    (global-set-key [remap yank-pop] 'helm-show-kill-ring)
    (global-set-key [remap apropos-command] 'helm-apropos)
    (global-set-key [remap query-replace-regexp] 'helm-regexp)
  :config
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))
  (add-to-list 'helm-sources-using-default-as-input
               'helm-source-man-pages)
  (dired-async-mode 1)
  (require 'helm-config)
  (setq helm-follow-mode-persistent t
        helm-reuse-last-window-split-state t
        helm-display-header-line nil
        helm-findutils-search-full-path t
        helm-show-completion-display-function nil
        helm-completion-mode-string ""
        helm-dwim-target 'completion
        helm-echo-input-in-header-line t
        helm-use-frame-when-more-than-two-windows nil
        helm-apropos-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-eshell-fuzzy-match t
        helm-imenu-fuzzy-match t
        helm-M-x-fuzzy-match t
        helm-recentf-fuzzy-match t
        helm-buffers-end-truncated-string "…"
        helm-buffer-max-length 22
        helm-window-show-buffers-function 'helm-window-mosaic-fn
        helm-window-prefer-horizontal-split t)
  (setq helm-mini-default-sources
        `(helm-source-buffers-list
          helm-source-recentf
          helm-source-bookmarks
          helm-source-bookmark-set
          helm-source-buffer-not-found))
  (helm-top-poll-mode)
  (helm-mode 1))

(use-package general
  :ensure t
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :no-autoload t
   :non-normal-prefix "M-SPC"
   "SPC" '(switch-to-previous-buffer :which-key "Toggle Buffer")
   "t" '(hydra-move/body :which-key "Move words")
   "b" '(hydra-buffer/body :which-key "Buffer")
   "p" '(hydra-projectile/body :which-key "Project")
   "f" '(helm-find-files :which-key "Find Files"))
  :preface
    (defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1))))

(use-package ace-window
  :defer t
  :ensure t
  :general
  (general-define-key
    :states '(normal)
    "SPC o" '(ace-window :which-key "Change buffer"))
  :bind (:map evil-normal-state-map ("M-o" . ace-window))
  :config
  (setq aw-keys '(?h ?j ?k ?l ?a ?s ?d ?f ?g)
        aw-dispatch-alist
        '((?x aw-delete-window "Delete Window")
          (?m aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?c aw-copy-window "Copy Window")
          (?J aw-switch-buffer-in-window "Select Buffer")
          (?n aw-flip-window)
          (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?C aw-split-window-fair "Split Fair Window")
          (?v aw-split-window-vert "Split Vert Window")
          (?b aw-split-window-horz "Split Horz Window")
          (?o delete-other-windows "Delete Other Windows")
          (?? aw-show-dispatch-help))))

(use-package which-key
  :defer t
  :ensure t
  :init (which-key-mode)
  :config
  (setq
    which-key-idle-secondary-delay 0
    which-key-allow-evil-operators t
    which-key-show-operator-state-maps t)
  (set-face-attribute 'which-key-local-map-description-face nil :weight 'bold)
  (which-key-setup-side-window-bottom))

(use-package expand-region
  :defer t
  :ensure t
  :bind
  (("M-p" . er/expand-region)
   ("M--" . er/contract-region)))

(use-package undo-fu
  :defer t
  :ensure t
  :general
  (:states 'normal
       "u" 'undo-fu-only-undo
       "C-r" 'undo-fu-only-redo))

(use-package projectile
  :defer t
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien))

(use-package ag
  :defer t
  :ensure t)

(use-package helm-ag
  :after helm
  :defer t
  :ensure t)

(use-package parrot
  :ensure t
  :config
  (parrot-mode)
  (add-to-list 'compilation-finish-functions
               'my/parrot-animate-when-compile-success)
  :preface
    (defun my/parrot-animate-when-compile-success (buffer result)
      (if (string-match "^finished" result)
          (parrot-start-animation))))

(use-package hydra
  :ensure t
  :defer t
  :config
  (defhydra hydra-buffer (:color blue :columns 3)
    "Buffers"
    ("n" next-buffer "next" :color red)
    ("b" helm-mini "switch")
    ("B" ibuffer "ibuffer")
    ("p" previous-buffer "prev" :color red)
    ("C-b" buffer-menu "buffer menu")
    ("N" evil-buffer-new "new")
    ("d" kill-this-buffer "delete" :color red)
    ("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red)
    ("s" save-buffer "save" :color red))
  (defhydra hydra-straight-helper (:color blue :columns 3)
    "Commands"
    ("c" straight-check-all "Check all")
    ("C" straight-check-package "Check package")
    ("r" straight-rebuild-all "Rebuild all")
    ("R" straight-rebuild-package "Rebuild package")
    ("f" straight-fetch-all "Fetch all")
    ("F" straight-fetch-package "Fetch package")
    ("p" straight-pull-all "Pull all")
    ("P" straight-pull-package "Pull package")
    ("m" straight-merge-all "Merge all")
    ("M" straight-merge-package "Merge package")
    ("n" straight-normalize-all "Normalize all")
    ("N" straight-normalize-package "Normalize package")
    ("u" straight-push-all "Push all")
    ("U" straight-push-package "Push package")
    ("v" straight-freeze-versions "Freeze versions")
    ("V" straight-thaw-versions "Thaw versions")
    ("w" straight-watcher-start "Watcher start")
    ("W" straight-watcher-quit "Watcher quit")
    ("g" straight-get-recipe "Get recipe")
    ("e" straight-prune-build "Prune build")
    ("q" nil))
  (defhydra hydra-projectile-other-window (:color teal :columns 1)
    "projectile-other-window"
    ("f"  projectile-find-file-other-window "file")
    ("g"  projectile-find-file-dwim-other-window "file dwim")
    ("d"  projectile-find-dir-other-window "dir")
    ("b"  projectile-switch-to-buffer-other-window "buffer")
    ("q"  nil "cancel" :color blue))
  ;; https://github.com/abo-abo/hydra/wiki/Projectile
  (defhydra hydra-projectile (:color teal :hint nil :columns 3)
    "Projectile"
    ("a" projectile-ag "ag" )
    ("b" projectile-switch-to-buffer "Switch to buffer")
    ("c" projectile-invalidate-cache "Clear cache")
    ("d" projectile-find-dir "Find directory")
    ("f" projectile-find-file "Find file")
    ("g" ggtags-update-tags "Update tags")
    ("i" projectile-ibuffer "ibuffer")
    ("K" projectile-kill-buffers "Kill buffers")
    ("m" projectile-multi-occur "Multi occur")
    ("p" projectile-switch-project "Switch Project")
    ("r" projectile-recentf "Recentf")
    ("x" projectile-remove-known-project "Remove known project")
    ("X" projectile-cleanup-known-projects "Cleanup known projects")
    ("z" projectile-cache-current-file "Cache current file")
    ("`" hydra-projectile-other-window/body "Other Window")
    ("q" nil "Cancel" :color blue))
  (defhydra hydra-move (:color red :columns 1)
    "Tranpose"
    ("s" transpose-sexps "Transpose Sexps")
    ("w" transpose-words "Transpose Words")
    ("u" move-text-up "Move Text Up")
    ("d" move-text-down "Move Text Down"))
  )

(use-package move-text
  :defer t
  :ensure t)

;; Org Mode
(use-package org
  :defer t
  :ensure t
  :config
  (setq org-startup-with-inline-images t
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-preserve-indentation t
        org-confirm-babel-evaluate nil
        org-return-follows-link t
        org-startup-truncated nil)
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (use-package ox-latex
    :straight nil
    :ensure nil
    :config
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)
    (setq org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
    )
  )

(use-package ob-python
  :straight nil
  :defer t
  :commands (org-babel-execute:python))

(use-package ob-shell
  :straight nil
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package org-tempo
  :straight nil
  :ensure nil
  :after org)

(use-package org-bullets
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package pdf-tools
  :defer t
  :ensure t
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (pdf-tools-install))

(use-package avy
  :defer t
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
              ("f" . avy-goto-word-1)
              ("F" . avy-goto-word-0))
  :config
  (setq avy-background t))

(use-package company
  :defer t
  :ensure t
  :init
  (setq company-idle-delay 0.3)
  (global-company-mode))

(use-package magit
  :after evil
  :defer t
  :ensure t)

(use-package transient
  :after magit
  :defer t
  :ensure t)

(use-package evil-magit
  :after transient
  :defer t
  :ensure t)

(use-package magit-todos
  :after magit
  :defer t
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "/usr/bin/pandoc"))

(use-package restart-emacs
  :defer t
  :ensure t)

(use-package diminish
  :defer t
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config
    :straight nil)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

(use-package evil-smartparens
  :defer t
  :ensure t
  :config
  (diminish 'evil-smartparens-mode))

;; Functions
(defun remote-connect ()
  (interactive)
  (let* ((user-name (read-string "User: "))
	 (ip-address (read-string "IP: "))
	 (dir (format "/home/%s" user-name)))
    (dired (format "/ssh:%s@%s:%s" user-name ip-address dir))))

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
