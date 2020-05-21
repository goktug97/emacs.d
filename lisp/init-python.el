(use-package elpy
  :ensure t
  :defer t
  :init (advice-add 'python-mode :before 'elpy-enable)
  :config (setq elpy-modules (delete 'elpy-module-flymake elpy-modules)
                elpy-rpc-timeout 10
                highlight-indentation-mode -1)
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1))))

(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(require 'color)
(use-package ein
  :ensure t
  :config
  (add-hook 'ein:markdown-mode-hook
            (lambda ()
              (progn 
                (setq ein:output-area-inlined-images t)
                (set-face-background
                 'ein:cell-input-area
                 (color-rgb-to-hex 
                  (/ (- (elt (color-values (face-attribute 'default :background)) 0)
                        1000)
                     65535.0)
                  (/ (- (elt (color-values (face-attribute 'default :background)) 1)
                        1000)
                     65535.0)
                  (/ (- (elt (color-values (face-attribute 'default :background)) 2)
                        1000)
                     65535.0)
                  2))))))

;; (add-hook 'python-mode-hook '(lambda () (flymake-mode)))
                     
