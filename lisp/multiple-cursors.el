;; (use-package multiple-cursors
;;   :ensure t
;;   :bind (("C-c e" . mc/edit-lines)
;; 	 ("C->" . mc/mark-next-like-this)
;; 	 ("C-<" . mc/mark-previous-like-this)
;; 	 ("C-c C-<" . mc/mark-all-like-this)))

(use-package evil-mc
  :ensure t
  :init (global-evil-mc-mode 1))
