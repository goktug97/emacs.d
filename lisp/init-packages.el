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

;; M-x all-the-icons-install-fonts
(use-package all-the-icons
  :defer t
  :ensure t)

(use-package dashboard
  :init
    (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-init-info t)
  (setq dashboard-items '()))

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

(use-package doom-modeline
  :defer t
  :after doom-themes
  :ensure t
  :hook (after-init . doom-modeline-mode))

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
        evil-want-C-i-jump nil
        evil-want-integration t
        evil-want-keybinding nil)
  (setq-default evil-symbol-word-search t)
  :config
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs))

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
   "a" '(hydra-gtd/body :which-key "GTD")
   "t" '(treemacs-select-window :which-key "Treemacs")
   "s" '(hydra-smartparens/body :which-key "Smartparens")
   "w" '(hydra-perspective-with-helm :which-key "Perspective")
   "g" '(magit :which-key "Magit")
   "m" '(hydra-move/body :which-key "Move words")
   "b" '(helm-mini :which-key "Buffers")
   "p" '(hydra-projectile-with-helm :which-key "Project")
   "f" '(helm-find-files :which-key "Find Files"))
  :preface
    (defun switch-to-previous-buffer ()
      (interactive)
      (switch-to-buffer (other-buffer (current-buffer) 1)))
    (defun hydra-projectile-with-helm ()
      (interactive)
      (require 'helm)
      (hydra-projectile/body))
    (defun hydra-perspective-with-helm ()
      (interactive)
      (require 'helm)
      (hydra-persp/body)))

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
  :after helm
  :defer t
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'helm)
  (setq projectile-indexing-method 'alien))

(use-package perspective
  :after helm
  :config
  (setq persp-sort 'created)
  (persp-mode))

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
  (defhydra hydra-smartparens (:columns 3 :hint nil)
    "Smartparens"
    ("(" (sp-wrap-with-pair "(") "Braces")
    ("{" (sp-wrap-with-pair "{") "Curly Braces")
    ("'" (sp-wrap-with-pair "'") "Apostrophe")
    ("\"" (sp-wrap-with-pair "\"") "Quote")
    ("w" (sp-wrap-with-pair "(") "Wrap")
    ("W" sp-unwrap-sexp "Unwrap")
    ("u" undo-fu-only-undo "Undo")
    ("s" sp-forward-slurp-sexp "Forward Slurp")
    ("S" sp-backward-slurp-sexp "Backward Slurp")
    ("b" sp-forward-barf-sexp "Forward Barf")
    ("B" sp-backward-barf-sexp "Backward Barf")
    ("q" nil))
  (defhydra hydra-persp (:color blue :columns 3)
    "Perspective"
    ("s" persp-switch "Switch")
    ("`" persp-switch-by-number "Switch by number")
    ("k" persp-remove-buffer "Remove buffer")
    ("c" persp-kill "Kill")
    ("r" persp-rename "Rename")
    ("a" persp-add-buffer "Add buffer")
    ("A" persp-set-buffer "Set buffer")
    ("b" persp-switch-to-buffer "Switch (all buffers)")
    ("i" persp-import "Import")
    ("n" persp-next "Next" :color red)
    ("p" persp-prev "Prev" :color red)
    ("S" persp-state-save "Save")
    ("L" persp-state-load "Load")
    ("q" nil))
  (defhydra hydra-org-clock (:color blue)
    "Org Clock"
    ("i" org-clock-in "Clock In")
    ("o" org-clock-out "Clock Out"))
  (defhydra hydra-org-timestamp (:color blue)
    ("p" org-time-stamp "Plain")
    ("s" org-schedule "Scheduled")
    ("d" org-deadline "Deadline")
    ("i" org-time-stamp-inactive "Inactive"))
  (defhydra hydra-gtd (:color blue)
    "GTD"
    ("a" org-agenda "Agenda")
    ("c" org-capture "Capture")
    ("C" hydra-org-clock/body "Org Clock")
    ("t" hydra-org-timestamp/body "Timestamp"))
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
        org-startup-truncated nil
        org-directory "~/goktug/org/"
        org-agenda-hide-tags-regexp "."
        org-log-done 'time
        org-use-fast-todo-selection t
        )
  (setq org-agenda-files
        (mapcar 'file-truename
                (file-expand-wildcards (concat org-directory "*.org"))))
  (setq org-refile-targets
        '(("projects.org" :regexp . "\\(?:\\(?:Note\\|Task\\)s\\)"))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        )
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (todo   . " %i %-12:c")
          (tags   . " %i %-12:c")
          (search . " %i %-12:c")))
  (setq org-agenda-custom-commands
        '(("g" "Get Things Done (GTD)"
           ((agenda ""
                    ((org-agenda-skip-function
                      '(org-agenda-skip-entry-if 'deadline))
                     (org-deadline-warning-days 0)))
            (todo "NEXT"
                  ((org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-prefix-format "  %i %-12:c [%e] ")
                   (org-agenda-overriding-header "\nTasks\n")))
            (agenda nil
                    ((org-agenda-entry-types '(:deadline))
                     (org-agenda-format-date "")
                     (org-deadline-warning-days 7)
                     ; (org-agenda-skip-function
                     ;  '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                     (org-agenda-overriding-header "\nDeadlines")))
            (tags-todo "inbox"
                       ((org-agenda-prefix-format "  %?-12t% s")
                        (org-agenda-overriding-header "\nInbox\n")))
            (tags "CLOSED>=\"<today>\""
                  ((org-agenda-overriding-header "\nCompleted today\n")))))))
  (setq org-capture-templates
        `(("i" "Inbox" entry  (file "inbox.org")
           ,(concat "* TODO %?\n"
                    "/Entered on/ %U"))
          ("m" "Meeting" entry  (file+headline "agenda.org" "Future")
           ,(concat "* %? :meeting:\n"
                    "<%<%Y-%m-%d %a %H:00>>"))
          ))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "HOLD(h)" "|" "DONE(d)")))
  (setcar (nthcdr 4 org-emphasis-regexp-components) 20)
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)

  (make-directory org-directory t)
  (unless (file-exists-p (concat org-directory "inbox.org"))
    (copy-file (concat user-init-dir "lisp/files/inbox.org") org-directory))
  (unless (file-exists-p (concat org-directory "agenda.org"))
    (copy-file (concat user-init-dir "lisp/files/agenda.org") org-directory))
  (unless (file-exists-p (concat org-directory "projects.org"))
    (copy-file (concat user-init-dir "lisp/files/projects.org") org-directory))
  (use-package ox-latex
    :straight nil
    :ensure nil
    :config
    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (setq org-latex-listings 'minted)
    (setq org-latex-pdf-process
          '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
  :preface
  (defun org-capture-inbox ()
    (interactive)
    ;;(call-interactively 'org-store-link)
    (org-capture nil "i"))
  (defun my/delete-capture-frame (&rest _)
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-frame)))
  (advice-add 'org-capture-finalize :after #'my/delete-capture-frame)
  (defun my/org-capture-frame ()
    (interactive)
    (require 'cl-lib)
    (select-frame-by-name "capture")
    (delete-other-windows)
    (cl-letf (((symbol-function 'switch-to-buffer-other-window) #'switch-to-buffer))
      (condition-case err
          (org-capture)
        (user-error (when (string= (cadr err) "Abort")
                      (delete-frame))))))
  (defun log-todo-next-creation-date (&rest ignore)
    "Log NEXT creation time in the property drawer under the key 'ACTIVATED'"
    (when (and (string= (org-get-todo-state) "NEXT")
               (not (org-entry-get nil "ACTIVATED")))
      (org-entry-put nil "ACTIVATED" (format-time-string "[%Y-%m-%d]"))))
  (add-hook 'org-after-todo-state-change-hook #'log-todo-next-creation-date)
  ;; Save the corresponding buffers
  (defun gtd-save-org-buffers ()
    "Save `org-agenda-files' buffers without user confirmation.
See also `org-save-all-org-buffers'"
    (interactive)
    (message "Saving org-agenda-files buffers...")
    (save-some-buffers t (lambda ()
                           (when (member (buffer-file-name) org-agenda-files)
                             t)))
    (message "Saving org-agenda-files buffers... done"))

  ;; Add it after refile
  (advice-add 'org-refile :after
              (lambda (&rest _)
                (gtd-save-org-buffers)))
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

(straight-use-package 'prescient)
(straight-use-package 'company-prescient)

(use-package company
  :defer t
  :ensure t
  :init
  (setq company-idle-delay 0.3)
  (global-company-mode)
  (company-prescient-mode))

(use-package magit
  :after evil
  :defer t
  :ensure t)

(use-package transient
  :after magit
  :ensure t)

(use-package hl-todo
  :ensure t
  :init
  (global-hl-todo-mode))

(use-package magit-todos
  :after magit
  :config
  (magit-todos-mode +1)
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
  :ensure t
  :config
  (setq restart-emacs-restore-frames t))

(use-package diminish
  :defer t
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (use-package smartparens-config
    :straight nil)
  (setq sp-python-insert-colon-in-function-definitions nil)
  (add-hook 'prog-mode-hook #'smartparens-mode)
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  :bind (:map smartparens-mode-map
              ("M-l" . sp-forward-slurp-sexp)
              ("M-h" . sp-backward-slurp-sexp)))


(use-package evil-smartparens
  :defer t
  :ensure t
  :config
  (diminish 'evil-smartparens-mode))

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))

;; (use-package paredit
;;   :bind (:map paredit-mode-map
;;               ("C-M-h" . paredit-forward-barf-sexp)
;;               ("C-M-l" . paredit-forward-slurp-sexp))
;;   :ensure t
;;   :defer t
;;   :init
;;   (autoload 'enable-paredit-mode "paredit" t)
;;   (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
;;   (add-hook 'eval-expression-minibuffer-setup-hook
;;             #'enable-paredit-mode)
;;   (add-hook 'ielm-mode-hook #'enable-paredit-mode)
;;   (add-hook 'lisp-mode-hook #'enable-paredit-mode)
;;   (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))

(use-package treemacs
  :defer t
  :ensure t)

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-perspective
  :after (treemacs persp-mode)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package speed-type
  :ensure t
  :defer t)

;; (use-package lsp-mode
;;   :ensure t
;;   :hook
;;   ((python-mode . lsp)))

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

(eval-after-load 'python
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

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

(require 'zone)
(defun zone-choose (pgm)
  "Choose a PGM to run for `zone'."
  (interactive
   (list
    (completing-read
     "Program: "
     (mapcar 'symbol-name zone-programs))))
  (let ((zone-programs (list (intern pgm))))
    (zone)))
