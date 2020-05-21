(use-package evil
  :ensure t
  :init 
  (setq evil-search-module 'evil-search
        evil-cross-lines t
        evil-move-beyond-eol t
        evil-move-cursor-back nil
        evil-want-fine-undo t
        evil-ex-complete-emacs-commands nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-shift-round nil
        evil-want-C-u-scroll t
        evil-want-integration t
        evil-want-keybinding nil)
  (setq-default evil-symbol-word-search t)
  :config 
  (evil-mode 1)
  (evil-set-initial-state 'term-mode 'emacs)
  (setq evil-mode-line-format nil)
  (setq evil-default-modeline-color
        (cons (face-background 'mode-line)
              (or (face-foreground 'mode-line)
                  "white"))))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Go-to-definition
(evil-global-set-key
 'normal "gd"
 (lambda () (interactive)
   (evil-execute-in-emacs-state)
   (call-interactively (key-binding
			(kbd "M-.")))))

(defun evil-color-modeline ()
  (let ((color
	 (cond
	  ((minibufferp) evil-default-modeline-color)
	  ((evil-insert-state-p)
	   '("#006fa0" . "#ffffff"))
	  ((evil-emacs-state-p)
	   '("#444488" . "#ffffff"))
	   (t evil-default-modeline-color))))
  (set-face-background 'mode-line (car color))
  (set-face-foreground 'mode-line (cdr color))))

(add-hook 'post-command-hook 'evil-color-modeline)

