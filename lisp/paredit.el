(use-package paredit
  :bind (:map paredit-mode-map
	      ("C-M-h" . paredit-forward-barf-sexp)
	      ("C-M-l" . paredit-forward-slurp-sexp))
  :ensure t)
