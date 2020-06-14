(use-package paredit
  :bind (:map paredit-mode-map
              ("C-M-h" . paredit-forward-barf-sexp)
              ("C-M-l" . paredit-forward-slurp-sexp))
  :ensure t
  :config
  (autoload 'enable-paredit-mode "paredit" t)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook
            #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode))
