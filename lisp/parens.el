(use-package smartparens
  :bind (:map smartparens-mode-map
              ("C-M-h" . sp-forward-barf-sexp)
              ("C-M-l" . sp-forward-slurp-sexp)
              ("M-(" . sp-wrap-round))
  :ensure t)
(require 'smartparens-config)
(add-hook 'python-mode-hook #'smartparens-mode)
