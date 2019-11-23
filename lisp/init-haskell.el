(require 'haskell-interactive-mode)
(require 'haskell-process)

(use-package hindent
  :ensure t)

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
     '(haskell-process-log t))))

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

(add-hook 'haskell-mode-hook #'haskell-mode-setup-hook)
(add-hook 'haskell-mode-hook #'evil-haskell)
(add-hook 'haskell-mode-hook #'hindent-mode)
