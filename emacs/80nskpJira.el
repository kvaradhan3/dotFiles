(use-package org-jira
  :init
  ;; (setq jiralib-token
  ;; 	(cons "Authorization"
  ;; 	      (concat "Bearer " (auth-source-pick-first-password
  ;; 				 :host "netskope.atlassian.net"))))

  (setq url-debug "all"
        request-log-level 'blather
        request-message-level 'blather) 
  (setq jiralib2-url			"https://netskope.atlassian.net"
	jiralib2-auth			'basic
	jiralib2-user-login-name	"kvaradhan@netskope.com"
	jiralib2-token			nil

	org-jira-working-dir		"~/org/jira"
	org-jira-project-filename-alist '(("RBI"  . "RBI")
					  ("ENG"  . "ENG")
					  ("SRE"  . "SRE")
					  ("OPS"  . "OPS")
					  ("OBS"  . "OBS"))
	org-jira-users '(("Kannan Varadhan" . "kvaradhan@netskope.com"))

	org-jira-priority-to-org-priority-alist '(("Blocker"	    . ?A)
						  ("Critical"	    . ?B)
						  ("Minor"	    . ?C)
						  ("Trivial"	    . ?D)
						  ("Service Outage" . ?E)
						  ("Lowest"	    . ?F)))

  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  ;; (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  ;; (require 'ejira-agenda)

  (add-to-list 'org-agenda-files org-jira-working-dir)

  ;; Add an agenda view to browse the issues that
  ;; (org-add-agenda-custom-command
  ;;  '("j" "my jira issues"
  ;;    ((ejira-jql "resolution = unresolved and assignee = currentuser()"
  ;;                ((org-agenda-overriding-header "assigned to me")))))))
  )
