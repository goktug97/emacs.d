(use-package helm-mu
  :ensure t
  :config (setq helm-mu-default-search-string
		"(maildir:/karakasli/INBOX OR maildir:/karakaslisent"))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(use-package mu4e)

(define-key mu4e-main-mode-map (kbd "M-s") 'helm-mu)
(define-key mu4e-headers-mode-map (kbd "M-s") 'helm-mu)
(define-key mu4e-view-mode-map (kbd "M-s") 'helm-mu)
(define-key mu4e-main-mode-map (kbd "M-j") 'helm-mu-jump-maildir)
(define-key mu4e-headers-mode-map (kbd "M-j") 'helm-mu-jump-maildir)
(define-key mu4e-view-mode-map (kbd "M-j") 'helm-mu-jump-maildir)

(defun helm-mu-jump-maildir ()
  (interactive)
  (let ((maildir (helm-comp-read "Maildir: " (mu4e-get-maildirs))))
    (mu4e-headers-search (format "maildir:\"%s\"" maildir))))

(setq mu4e-get-mail-command "mbsync -a")
(setq mu4e-user-mail-address-list (list "karakasligk@gmail.com"
					"karakasli16@itu.edu.tr"))
(setq message-kill-buffer-on-exit t)
(setq mu4e-use-fancy-chars t)

(setq mu4e-sent-messages-behavior 'delete)

(setq mu4e-view-show-images t
      mu4e-view-image-max-width 800)

;; sending mail
(setq message-send-mail-function 'message-send-mail-with-sendmail
      sendmail-program "/usr/bin/msmtp"
      user-full-name "Göktuğ Karakaşlı")

(setq mu4e-confirm-quit nil
      mu4e-headers-date-format "%d/%b/%Y %H:%M" ; date format
      mu4e-html2text-command "python2 -m html2text"
      )

(defun choose-msmtp-account ()
  (if (message-mail-p)
      (save-excursion
	(let*
	    ((from (save-restriction
		     (message-narrow-to-headers)
		     (message-fetch-field "from")))
	     (account
	      (cond
	       ((string-match "karakasligk@gmail.com" from) "karakasli")
	       ((string-match "karakasli16" from) "itu"))))
	  (setq message-sendmail-extra-arguments (list '"-a" account))))))
(setq message-sendmail-envelope-from 'header)
(add-hook 'message-send-mail-hook 'choose-msmtp-account)

(setq mu4e-contexts
      `(
	,(make-mu4e-context
	  :name "itu"
	  :enter-func
	  (lambda ()
	    (mu4e-message "Entering ITU contenxt"))
	  :leave-func (lambda () (mu4e-message "Leaving ITU context"))
	  :vars
	  '((smtpmail-smtp-server . "outgoing.itu.edu.tr")
	    (smtpmail-default-smtp-server . "outgoing.itu.edu.tr")
	    (user-mail-address . "karakasli16@itu.edu.tr")
	    (smtpmail-smtp-service . 587)))
	,(make-mu4e-context
	  :name "karakasli"
	  :enter-func
	  (lambda ()
	    (mu4e-message "Entering GMail contenxt"))
	  :leave-func (lambda () (mu4e-message "Leaving GMail context"))
	  :vars
	  '((smtpmail-smtp-server . "smtp.gmail.com")
	    (smtpmail-default-smtp-server . "smtp.gmail.com")
	    (user-mail-address . "karakasligk@gmail.com")
	    (smtpmail-smtp-service . 587)))))
      
