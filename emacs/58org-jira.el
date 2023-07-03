;;;
;;; ORG ejira
;;;
;;; Reference: https://github.com/ahungry/org-jira
;;;

(use-package org-jira
    :init
    (setq jiralib-url              "https://netskope.atlassian.net")

    :bind (("C-x j"	. 'org-jira-get-issues))
    ;; :bind (
    ;; 	   :map org-jira-map
    ;; 		("C-c pg" .	org-jira-get-projects)
    ;; 		("C-c ib" .	org-jira-browse-issue)
    ;; 		("C-c ig" .	org-jira-get-issues)
    ;; 		("C-c ij" .	org-jira-get-issues-from-custom-jql)
    ;; 		("C-c ih" .	org-jira-get-issues-headonly)
    ;; 		("C-c iu" .	org-jira-update-issue)
    ;; 		("C-c iw" .	org-jira-progress-issue)
    ;; 		("C-c in" .	org-jira-progress-issue-next)
    ;; 		("C-c ia" .	org-jira-assign-issue)
    ;; 		("C-c ir" .	org-jira-refresh-issue)
    ;; 		("C-c iR" .	org-jira-refresh-issues-in-buffer)
    ;; 		("C-c ic" .	org-jira-create-issue)
    ;; 		("C-c ik" .	org-jira-copy-current-issue-key)
    ;; 		("C-c sc" .	org-jira-create-subtask)
    ;; 		("C-c sg" .	org-jira-get-subtasks)
    ;; 		("C-c cc" .	org-jira-add-comment)
    ;; 		("C-c cu" .	org-jira-update-comment)
    ;; 		("C-c wu" .	org-jira-update-worklogs-from-org-clocks)
    ;; 		("C-c tj" .	org-jira-todo-to-jira)
    ;; 		("C-c if" .	org-jira-get-issues-by-fixversion)
    ;; 		)

    :config
    (setq org-jira-custom-jqls
	  '(
	    (:jql (concat "project IN (EX, AHU) "
			  "AND createdDate < '2019-01-01' "
			  "order by created DESC ")
	     :limit 10
	     :filename "last-years-work")
	    (:jql (concat "project IN (EX, AHU)"
			  "AND createdDate >= '2019-01-01' "
			  "order by created DESC ")
		  :limit 10
		  :filename "this-years-work")
	    (:jql (concat "project IN (EX, AHU) "
			  "AND status IN ('To Do', 'In Development') "
			  "AND (labels = EMPTY OR labels NOT IN ('FutureUpdate')) "
			  "order by priority, created DESC ")
		  :limit 20
		  :filename "ex-ahu-priority-items")
	    ))
    )

;; ;; Add an agenda view to browse the issues that
;; (org-add-agenda-custom-command
;;  '("j" "My JIRA issues"
;;    ((ejira-jql "resolution = unresolved and assignee = currentUser()"
;;                ((org-agenda-overriding-header "Assigned to me"))))))
;; )
