(require 'haskell-interactive-mode)
(require 'haskell-process)

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

(add-hook 'haskell-mode-hook #'haskell-mode-setup-hook)
