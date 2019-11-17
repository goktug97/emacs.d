(require 'helm-config)

(use-package helm-descbinds
  :ensure t
  :init (helm-descbinds-mode))

;; (use-package wgrep-helm
;;   :ensure t
;;   :init 
;;   (setq wgrep-auto-save-buffer t
;; 	wgrep-enable-key (kbd "C-x C-q"))
;;   (add-hook 'wgrep-setup-hook #'wgrep-change-to-wgrep-mode))

(helm-mode 1)

(add-to-list 'helm-sources-using-default-as-input
	     'helm-source-man-pages)

(dired-async-mode)
(setq helm-follow-mode-persistent t
      helm-reuse-last-window-split-state t
      helm-display-header-line nil
      helm-findutils-search-full-path t
      helm-show-completion-display-function nil
      helm-completion-mode-string ""
      helm-dwim-target 'completion
      helm-echo-input-in-header-line t
      helm-use-frame-when-more-than-two-windows nil
      ; helm-grep-save-buffer-name-no-confirm t
      helm-apropos-fuzzy-match t
      helm-buffers-fuzzy-matching t
      helm-eshell-fuzzy-match t
      helm-imenu-fuzzy-match t
      helm-M-x-fuzzy-match t
      helm-recentf-fuzzy-match t
      helm-buffers-end-truncated-string "…"
      helm-buffer-max-length 22
      helm-window-show-buffers-function
      'helm-window-mosaic-fn
      helm-window-prefer-horizontal-split t)

(setq helm-mini-default-sources
      `(helm-source-buffers-list
	helm-source-recentf
	helm-source-bookmarks
	helm-source-bookmark-set
	helm-source-buffer-not-found))

(use-package helm-slime
  :ensure t
  :init
  (with-eval-after-load 'slime-repl
    (defun slime-set-keys ()
      ;;(define-key slime-repl-mode-map
      ;;  (kbd "M-p") 'helm-slime-repl-history)
      (define-key slime-repl-mode-map
	(kbd "M-s") nil)
      (define-key slime-autodoc-mode-map
	(kbd "C-c C-d C-a") 'helm-slime-apropos)
      (define-key slime-mode-map
	(kbd "M-<tab>") 'helm-slime-complete)
      (dolist (key '("M-<tab>" "<tab>"))
	(define-key slime-repl-mode-map
	  (kbd key) 'helm-slime-complete)))
    (add-hook 'slime-repl-mode-hook
	      'slime-set-keys)
    (add-to-list 'helm-source-names-using-follow "SLIME xrefs")))

(global-set-key [remap execute-extended-command] 'helm-M-x)
(global-set-key [remap find-file] 'helm-find-files)
(global-set-key [remap occur] 'helm-occur)
(global-set-key [remap list-buffers] 'helm-mini)
(global-set-key [remap yank-pop] 'helm-show-kill-ring)
(global-set-key [remap apropos-command] 'helm-apropos)
(global-set-key [remap query-replace-regexp] 'helm-regexp)

(unless (boundp 'completion-in-region-function)
  (define-key
    lisp-interaction-mode-map
    [remap completion-at-point]
    'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map
    [remap completion-at-point]
    'helm-lisp-completion-at-point))

(define-key prog-mode-map (kbd "M-s f") 'helm-semantic-or-imenu)
(define-key text-mode-map (kbd "M-s f") 'helm-semantic-or-imenu)

(with-eval-after-load 'woman
  (define-key woman-mode-map (kbd "M-s f") 'helm-imenu))
(with-eval-after-load 'man
  (define-key Man-mode-map (kbd "M-s f") 'helm-imenu))

(set-face-attribute 'helm-source-header nil
		    :inherit 'header-line
		    :height 'unspecified
		    :background 'unspecified
		    :foreground 'unspecified)
(set-face-background 'helm-selection "#4f4f4f")
(set-face-background 'helm-visible-mark "#2f2f2f")
(set-face-foreground 'helm-visible-mark nil)
(set-face-foreground 'helm-match "red")
(set-face-attribute 'helm-buffer-file nil
		    :background 'unspecified
		    :foreground "white"
		    :weight 'normal)
(set-face-attribute 'helm-buffer-directory nil
		    :background 'unspecified
		    :foreground "#1e90ff"
		    :weight 'bold)
(set-face-attribute 'helm-ff-directory nil
		    :background 'unspecified
		    :foreground 'unspecified
		    :weight 'unspecified
		    :inherit 'helm-buffer-directory)
(set-face-attribute 'helm-ff-file nil
		    :background 'unspecified
		    :foreground 'unspecified
		    :weight 'unspecified
		    :inherit 'helm-buffer-file)
; (set-face-foreground 'helm-grep-finish "#00AA00")

(defun helm-skip-dots (old-func &rest args)
  (apply old-func args)
  (let ((sel (helm-get-selection)))
    (if (and (stringp sel) (string-match "/\\.$" sel))
	(helm-next-line 2)))
  (let ((sel (helm-get-selection))) ; if we reached .. move back
    (if (and (stringp sel) (string-match "/\\.\\.$" sel))
	(helm-previous-line 1))))
(advice-add #'helm-preselect :around #'helm-skip-dots)
(advice-add #'helm-ff-move-to-first-real-candidate
	    :around #'helm-skip-dots)

(helm-top-poll-mode)

(global-set-key (kbd "C-<f4>") 'helm-execute-kmacro)
