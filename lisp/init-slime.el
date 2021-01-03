(use-package slime
  :ensure t
  :config
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  (setq inferior-lisp-program "/usr/bin/sbcl"
        slime-contribs '(slime-fancy))
  (add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1))))

(defun override-slime-repl-bindings-with-paredit ()
  (define-key slime-repl-mode-map
    (read-kbd-macro paredit-backward-delete-key) nil))
(add-hook 'slime-repl-mode-hook 'override-slime-repl-bindings-with-paredit)

(defvar slime-repl-font-lock-keywords lisp-font-lock-keywords-2)
(defun slime-repl-font-lock-setup ()
  (setq font-lock-defaults
        '(slime-repl-font-lock-keywords
         ;; From lisp-mode.el
         nil nil (("+-*/.<>=!?$%_&~^:@" . "w")) nil
         (font-lock-syntactic-face-function
         . lisp-font-lock-syntactic-face-function))))

(add-hook 'slime-repl-mode-hook 'slime-repl-font-lock-setup)

(defadvice slime-repl-insert-prompt (after font-lock-face activate)
  (let ((inhibit-read-only t))
    (add-text-properties
     slime-repl-prompt-start-mark (point)
     '(font-lock-face
      slime-repl-prompt-face
      rear-nonsticky
      (slime-repl-prompt read-only font-lock-face intangible)))))

(use-package helm-slime
  :ensure t
  :after helm
  :init
  (with-eval-after-load 'slime-repl
    (defun slime-set-keys ()
      (define-key slime-repl-mode-map
        (kbd "M-s") nil)
      (define-key slime-autodoc-mode-map
        (kbd "C-c C-d C-a") 'helm-slime-apropos)
      (define-key slime-mode-map
        (kbd "M-<tab>") 'helm-slime-complete)
      (dolist (key '("M-<tab>" "<tab>"))
        (define-key slime-repl-mode-map
          (kbd key) 'helm-slime-complete)))
    (add-hook 'slime-repl-mode-hook
              'slime-set-keys)
    (add-to-list 'helm-source-names-using-follow "SLIME xrefs")))
