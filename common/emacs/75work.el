;;;
;;; Work specific hooks
;;;

(use-package org-capture
  :straight nil
  :after org
  :config
  (defun confluence-encode (tag)
    "Convert spaces to + for confluence, then url-hexify-string"
    (let ((toks (split-string (string-trim tag) "/"))
	  (folder nil))
      (if (> (length toks) 1)
	  (setq folder (car toks)
		tag (string-join (cdr toks) "/")))
      (setq tag (replace-regexp-in-string
		 "%20" "+" (url-hexify-string
			    (string-trim tag))))
      (if folder
	  (setq tag (string-join (list folder tag) "/")))
      tag))

  (setq org-link-abbrev-alist
	(append org-link-abbrev-alist
		'(("jira"       . "https://jira.corp.zscaler.com/browse/")
		  ("ET"         . "https://confluence.corp.zscaler.com/display/ET/%(confluence-encode)")
		  ("confluence" . "https://confluence.corp.zscaler.com/display/%(confluence-encode)")
		  ("pageId"     . "https://confluence.corp.zscaler.com/pages/viewpage.action?pageId="))
		  ))))

;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:
