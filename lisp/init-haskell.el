(require 'haskell-interactive-mode)
(require 'haskell-process)
(require 'helm)
(require 'helm-buffers)

(use-package hindent
  :ensure t)

(defun evil-haskell ()
  (with-eval-after-load "haskell-mode"
    ;; This changes the evil "O" and "o" keys for haskell-mode to make sure that
    ;; indentation is done correctly. See
    ;; https://github.com/haskell/haskell-mode/issues/1265#issuecomment-252492026.
    (defun haskell-evil-open-above ()
      (interactive)
      (evil-digit-argument-or-evil-beginning-of-line)
      (haskell-indentation-newline-and-indent)
      (evil-previous-line)
      (haskell-indentation-indent-line)
      (evil-append-line nil))

    (defun haskell-evil-open-below ()
      (interactive)
      (evil-append-line nil)
      (haskell-indentation-newline-and-indent))

    (evil-define-key 'normal haskell-mode-map
      "o" 'haskell-evil-open-below
      "O" 'haskell-evil-open-above)
    )
  )

;; Adding helm support to interactive haskell
;; The haskell-interactive must be modified
;; because the history is defined as local and
;; it must be global.

(defgroup helm-haskell-interactive nil
  "Haskell Interactive for Helm"
  :group 'helm)

(defcustom helm-haskell-history-max-offset 400
  "Max number of chars displayed per candidate in `helm-haskell-interactive-history'."
  :type '(choice (const :tag "Disabled" t)
		 (integer :tag "Max candidate offset"))
  :group 'helm-haskell-interactive)

(defvar helm-haskell-source-interactive-mode-history
  (helm-build-sync-source "REPL history"
    :candidates (lambda ()
		  (with-helm-current-buffer
		    haskell-interactive-mode-history))
    :action 'haskell-interactive-mode-set-prompt
    :multiline 'helm-haskell-history-max-offset)
  "Source that provides Helm completion against `haskell-interactive-mode-history'.")

(defun helm-haskell-interactive-mode-history ()
  (interactive)
  (cond
   ((derived-mode-p 'haskell-interactive-mode)
    (helm :sources 'helm-haskell-source-interactive-mode-history
	  :input (word-at-point)
          :buffer "*helm Haskell history*"))))


(defun haskell-mode-setup-hook ()
  (interactive)
  (progn
    (turn-on-haskell-indent)
    (turn-on-haskell-doc)
    (turn-on-haskell-decl-scan)
    (interactive-haskell-mode)
    (custom-set-variables
     '(haskell-process-suggest-remove-import-lines t)
     '(haskell-process-auto-import-loaded-modules t)
     '(haskell-process-log t))
    (define-key haskell-interactive-mode-map
    (kbd "M-r") 'helm-haskell-interactive-mode-history)))

(add-hook 'haskell-mode-hook #'haskell-mode-setup-hook)
(add-hook 'haskell-mode-hook #'evil-haskell)
(add-hook 'haskell-mode-hook #'hindent-mode)
