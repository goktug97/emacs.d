(defun flycheck-haskell-config ()
  (progn
    (flycheck-mode)
    ;; (flycheck-select-checker 'haskell-hlint)
    (flycheck-select-checker 'haskell-ghc)
    (setq flycheck-ghc-args '("-Wall" "-dynamic"))))

(use-package flycheck
  :ensure t
  :init (add-hook 'haskell-mode-hook #'flycheck-haskell-config))
