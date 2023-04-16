;;;
;;; netSkope specific hooks
;;;

(use-package org-agenda
  :straight nil
  :config
  (setq org-agenda-files
        (append org-agenda-files
                '("~/org/"
                  "~/org/2022/"))))

(use-package org-capture
  :straight nil
  :after org
  :config
  (setq org-link-abbrev-alist
	(append org-link-abbrev-alist
		'(("jira" . "https://netskope.atlassian.net/browse/")
                  ("github" . "https://github.com/netSkope/%s")
                  ("github-pr" . "https://github.com/netSkope/%(github-pr)"))))
  (defun github-pr (tag)
    "Convert a github PR tag of <component>/<PR#> to <component>/pull/<PR#>"
    (string-join (split-string tag "/") "/pull/"))

  (setq org-capture-templates
	(append org-capture-templates
		(doct
		 '(
		   ("(W) Candidate Interview"
		    :keys "i"
		    :type plain
		    :file "~/org/interviews.org"
		    :clock-in t
		    :clock-resume t
		    :template ("* %^{Candidate} <%^{Email}> %^G"
			       ;; "  %^{Resume}p"
			       ;; "  %^{Phone}p"
			       "  %^{Greenhouse}p"
			       "  %^{Conferencing}p"
			       "  %^{CoderPad}p"
			       "  %^{Position}p"
			       "  "
			       "  - Resume"
			       "    %?"
			       ""
			       "  - Summary"
			       ""
			       "  Clock Hints {Delete when Finalizing}"
			       "  C-c C-x C-i		/org-clock-in/"
			       "  C-c C-x C-o		/org-clock-out/"
			       "  C-c C-x C-q		/org-clock-cancel/"
			       "  C-c C-x C-j		/org-clock-goto/"
			       "        {and cancel/out that clock}"
			       "  C-c C-x C-d		/org-clock-display/"
			       )
		    :hook (lambda ()
			    (my/org-capture-add-tags '("@work" "interview")))
		    )
 		   ("(W) Note"
		    :keys "n"
		    :type item
		    :file "~/org/projects.org"
		    :function (lambda ()
				(let ((org-goto-interface 'outline-path-completion))
                                  (org-goto))
				)
		    :clock-in t
		    :clock-resume t
		    :template ("%^{Title} %^G"
			       "%U"
			       ":PROPERTES:"
			       ":Attendees: %^{Attendees}"
			       ":END:"
			       "%?")
		    :hook (lambda ()
			    (my/org-capture-add-tags '("@work")))
		    )
		   ("(W) Todo"
		    :keys "x"
		    :type plain
		    :file "~/org/inbox.org"
		    :regexp "Tasks"
		    :template ("* TODO %^{Task}"
			       "  SCHEDULED: %^{Due Date}T"
			       "  %?")
		    :hook (lambda ()
			    (my/org-capture-add-tags '("@work")))
		    )
		   ))))
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
