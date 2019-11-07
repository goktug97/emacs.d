(defun flycheck-haskell-config ()
  (progn
    (flycheck-mode)
    ;; (flycheck-select-checker 'haskell-hlint)
    (setq flycheck-ghc-args '("-Wall" "-dynamic"))))

(use-package flycheck
  :ensure t
  :init (add-hook 'haskell-mode-hook #'flycheck-haskell-config))
