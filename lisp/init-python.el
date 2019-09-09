(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config (setq elpy-modules (delete 'elpy-module-flymake elpy-modules)))
