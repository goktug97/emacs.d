(use-package magit
  :after evil
  :ensure t)

(use-package transient
  :after magit
  :ensure t)

(use-package evil-magit
  :after transient
  :ensure t)

(use-package magit-todos
  :after magit
  :ensure t)

(defmethod helm-setup-user-source ((source helm-source-ffiles))
  (helm-source-add-action-to-source-if
   "Magit status"
   (lambda (_candidate)
     (magit-status helm-ff-default-directory))
   source
   (lambda (candidate)
     (and (not (string-match-p ffap-url-regexp candidate))
	  helm-ff-default-directory
	  (locate-dominating-file
	   helm-ff-default-directory ".git")))
   1))
