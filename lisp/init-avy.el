(use-package avy
  :ensure t
  :after evil
  :bind (:map evil-normal-state-map
              ("f" . avy-goto-word-1)
              ("F" . avy-goto-word-0))
  :config
  (setq avy-background t))
