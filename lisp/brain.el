(use-package emacsql-sqlite
  :ensure t)

(use-package emacsql-sqlite3
  :ensure t)

(use-package org-roam
  :after org
  :ensure nil
  ;; :ensure t
  :hook (after-init . org-roam-mode)
  :load-path "~/libraries/org-roam"
  :custom (org-roam-directory "~/Brain/")
  :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-show-graph)
               ("C-c n r" . org-roam-db-build-cache))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))))

(require 'org-roam-protocol)

(use-package org-roam-server
  :ensure nil
  :after org-roam
  :load-path "~/libraries/org-roam-server/")

;; ;; For Testing
;; (use-package org-roam-server
;;   :ensure nil
;;   :config
;;   (setq org-roam-server-authenticate nil)
;;   :after org-roam
;;   :load-path "/tmp/org-roam-server/")

;; (use-package org-roam-server
;;   :after org-roam
;;   :ensure t)

(use-package company-org-roam
  :ensure t
  :after org-roam
  :config (push 'company-org-roam company-backends))

(use-package org-journal
  :ensure t
  :after org
  :bind
  ("C-c n j" . org-journal-new-entry)
  :custom
  (org-journal-date-prefix "#+TITLE: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/Brain")
  (org-journal-date-format "%A, %d %B %Y"))

(use-package org-ref
  :ensure t
  :after org
  :config
  (setq org-ref-default-bibliography '("/home/goktug/bibliography/brain.bib")
        org-ref-pdf-directory "/home/goktug/bibliography/"
        bibtex-completion-notes-path "/home/goktug/Brain/"
        bibtex-completion-bibliography "/home/goktug/bibliography/brain.bib"
        bibtex-completion-library-path "/home/goktug/bibliography"
        bibtex-completion-pdf-field "file"
        bibtex-completion-pdf-open-function 'org-open-file))

(use-package helm-bibtex
  :ensure t)

(use-package org-roam-bibtex
  :ensure t
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :bind (:map org-mode-map
              (("C-c n a" . orb-note-actions)))
  :config
  (setq orb-preformat-keywords
        '(("citekey" . "=key=") "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_KEY: ${ref}

- tags ::
- keywords :: ${keywords}

* ${title}
:PROPERTIES:
:Custom_ID: ${citekey}
:URL: ${url}
:AUTHOR: ${author-or-editor}
:NOTER_DOCUMENT: %(orb-process-file-field \"${citekey}\")
:NOTER_PAGE:
:END:"))))

(use-package org-noter
  :after org
  :ensure t
  :bind (:map org-noter-doc-mode-map
              (("C-c i" . org-noter-insert-note)))
  :config
  (setq
   org-noter-always-create-frame nil
   org-noter-hide-other nil
   org-noter-doc-split-fraction '(0.7 0.3)
   org-noter-notes-search-path (list "~/Brain/")))

;; Agenda 
(defun org-skip-subtree-if-priority (priority)
  (let ((subtree-end (save-excursion (org-end-of-subtree t)))
        (pri-value (* 1000 (- org-lowest-priority priority)))
        (pri-current (org-get-priority (thing-at-point 'line t))))
    (if (= pri-value pri-current)
        subtree-end
      nil)))

(setq org-agenda-files '("~/Brain/"))
(setq org-agenda-custom-commands
      '(("c" "Simple agenda view"
         ((tags "PRIORITY=\"A\""
                ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header "High-priority unfinished tasks:")))
          (agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(or (org-skip-subtree-if-priority ?A)
                          (org-agenda-skip-if nil '(scheduled deadline))))))))))

(add-hook 'org-agenda-mode-hook
	  (lambda ()
	    (define-key org-agenda-mode-map "j" 'org-agenda-next-item)
	    (define-key org-agenda-mode-map "k" 'org-agenda-previous-item)))

(defun pop-to-org-agenda (&optional split)
  "Visit the org agenda, in the current window or a SPLIT."
  (interactive "P")
  (org-agenda nil "c")
  (when (not split)
    (delete-other-windows)))
    
(define-key evil-normal-state-map (kbd "S-SPC") 'pop-to-org-agenda)

(setq org-roam-graph-executable "/usr/bin/neato")
(setq org-roam-graph-extra-config '(("overlap" . "false")))
(setq org-roam-completion-system 'helm)

(defun brain-screenshot ()
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          (concat "roam_imgs/"
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (sleep-for 0 500)
  (unless (file-exists-p (file-name-directory filename))
    (make-directory (file-name-directory filename)))
      (call-process "maim" nil nil nil "-s" filename)
  (if (file-exists-p filename)
      (insert (concat "[[file:" filename "]]")))
  (org-redisplay-inline-images))

(defun my/org-roam--backlinks-list-with-content (file)
  (with-temp-buffer
    (if-let* ((backlinks (org-roam--get-backlinks file))
              (grouped-backlinks (--group-by (nth 0 it) backlinks)))
        (progn
          (insert (format "\n\n* %d Backlinks\n"
                          (length backlinks)))
          (dolist (group grouped-backlinks)
            (let ((file-from (car group))
                  (bls (cdr group)))
              (insert (format "** [[file:%s][%s]]\n"
                              file-from
                              (org-roam--get-title-or-slug file-from)))
              (dolist (backlink bls)
                (pcase-let ((`(,file-from _ ,props) backlink))
                  (insert (s-trim (s-replace "\n" " " (plist-get props :content))))
                  (insert "\n\n")))))))
    (buffer-string)))

  (defun my/org-export-preprocessor (backend)
    (let ((links (my/org-roam--backlinks-list-with-content (buffer-file-name))))
      (unless (string= links "")
        (save-excursion
          (goto-char (point-max))
          (insert (concat "\n* Backlinks\n") links)))))

  (add-hook 'org-export-before-processing-hook 'my/org-export-preprocessor)
