;;;
;;; Org Mode
;;;

(setq work-organizer (expand-file-name "~/org/inbox.org")
      home-organizer (expand-file-name "~/Drives/Sync/Agenda/inbox.org")
      dflt-organizer (expand-file-name "~/Drives/Sync/Agenda/gtd-inbox.org")
      )

(set-register ?o (cons 'file work-organizer))
(set-register ?h (cons 'file home-organizer))

(use-package org
  :mode ("\\.org\\'"            . org-mode)
  :mode ("\\.org_archive\\'"    . org-mode)
  :mode ("\\.txt\\'"            . org-mode)
  :bind (("C-c l"       . org-store-link)
         ("C-c b"       . org-switchb))
  :config
  (setq org-agenda-files        '()
        org-capture-templates   '()
        org-refile-targets      '((nil :maxlevel . 9))
        org-completion-use-ido  nil)

  ;;; https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html
  (setq org-refile-use-outline-path             'file
        org-outline-path-complete-in-steps      nil
        org-refile-allow-creating-parent-nodes  'confirm)
  (setq org-adapt-indentation t
	org-hide-leading-stars t
	org-odd-levels-only t)

  (setq org-link-abbrev-alist
	'(
	  ("rfc"  . "http://tools.ietf.org/html/rfc")
	  ))

  (add-hook 'org-mode-hook
	    (lambda () 
	      (turn-on-auto-fill)))
  )

(use-package org-crypt
  :straight nil
  :after org
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  (setq org-crypt-key "kannan@ieee.org")
  )

;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:

