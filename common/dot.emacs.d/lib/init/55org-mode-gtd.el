;;;-
;;; Some library functions for Org Mode
;;;

;;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
;;; someday see http://members.optusnet.com.au/~charles57/GTD/orgmode.html

(use-package org-edna)

(use-package org-gtd
  :after org
  :ensure t
  :init
  (setq org-gtd-update-ack "4.0.0"
	org-gtd-directory  "~/NOTES/TASKS")

  :custom
  (org-todo-keywords '((sequence "TODO" "NEXT" "WAIT" "|" "DONE" "CNCL")))
  (org-gtd-keyword-mapping '((todo . "TODO")       ; tasks not ready to act on
			     (next . "NEXT")       ; tasks ready to act on immediately
			     (wait . "WAIT")       ; tasks blocked or delegated
			     (done . "DONE")       ; tasks successfully completed
			     (canceled . "CNCL"))) ; tasks that won't be completed

  ;; Enable per-type refile prompting (recommended)
  ;; Without this, all items auto-refile to first target without prompting
  (setq org-gtd-refile-to-any-target nil)
  ;; Customize which types prompt vs auto-refile:
  ;; (setq org-gtd-refile-prompt-for-types '(project-heading project-task ...))  :config

  :config
  (org-edna-mode 1)

  ;; These only work after org-gtd loads (e.g. so org-gtd-directory is defined)
  (with-eval-after-load 'org-gtd
    (setq org-agenda-files (list org-gtd-directory))
    (define-key org-gtd-clarify-mode-map (kbd "C-c c") 'org-gtd-organize))

  ;; Quick task actions in agenda view
  (with-eval-after-load 'org-agenda
    (define-key org-agenda-mode-map (kbd "C-c .") 'org-gtd-agenda-transient))

  :bind
  (("C-c d c"		. org-gtd-capture)
   ("C-c d e"		. org-gtd-engage)
   ("C-c d p"		. org-gtd-process-inbox)
   ("C-c d n"		. org-gtd-show-all-next)
   ("C-c d s"		. org-gtd-reflect-stuck-projects)

   :map org-gtd-clarify-mode-map
   ("C-c c"	. org-gtd-organize)

   :map org-agenda-mode-map
   ("C-c ."	. org-gtd-agenda-transient))
  )


;; ;; split GTD in four separate files:
;; ;; - [inbox.org]:   collect everything;
;; ;; - [gtd.org]:     all my projects;
;; ;; - [someday.org:  All inactive tasks that I might do at some point
;; ;;                  in the future, but don’t want to see all the time;
;; ;; - [tickler.org]: I put entries in this file with a timestamp to get
;; ;;                  reminded at the right moment.
;; (use-package org-agenda
;;   :straight nil
;;   :after org
;;   :bind
;;   ("C-c a"        . org-agenda)
;;   :config
;;   (setq org-log-done    t)
;;   (setq org-agenda-files
;;         (append org-agenda-files
;;                 '("~/Drives/Sync/Agenda/")))
;;   (setq org-refile-targets
;;         (append org-refile-targets
;;                 '((org-agenda-files :maxlevel . 3))))

;;   (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|"
;;                                       "DONE(d)" "CANCELLED(c)")))

;;   ;; C-c C-s on an sntry to add a schedule
;;   ;; C-c C-d to enter a deadline

;;   ;; Contexts:  @work, @home, @personal, @adhithya, @adhvik
;;   (setq org-agenda-custom-commands
;;         '(("o" "Things related to Work, Office" tags-todo "@work"
;;            ((org-agenda-overriding-header "work")
;;             (org-agenda-skip-function #'gtd-skip-all-siblings-but-first)))

;;           ("p" "Professional:  work + Personal development"
;;            tags-todo "@professional"
;;            ((org-agenda-overriding-header "professional")
;;             (org-agenda-skip-function #'gtd-skip-all-siblings-but-first)))

;;           ("h" "Home Activities"
;;            tags-todo "@home"
;;            ((org-agenda-overriding-header "home")
;;             (org-agenda-skip-function #'gtd-skip-all-siblings-but-first)))
;; 	  ))

;;   (org-agenda-list nil)
;;   (run-with-idle-timer 300 t
;;                        (lambda ()
;;                          (save-window-excursion
;;                            (org-agenda nil "a")
;;                            (org-save-all-org-buffers))))
;;   )

;; (defun my/org-capture-add-tags (canned-tags)
;;   "Add CANNED-TAGS to an entry in the capture buffer."
;;   (save-excursion
;;     (save-restriction
;;       (org-narrow-to-subtree)
;;       (goto-char (point-min))
;;       (let ((my/org-tags-for-this-entry (org-get-tags nil t)))
;; 	(dolist (tag (mapcar #'string-trim canned-tags))
;; 	  (message "processing %s" tag)
;; 	  (if (not (member tag my/org-tags-for-this-entry))
;; 	      (setq my/org-tags-for-this-entry
;; 		    (append my/org-tags-for-this-entry
;; 			    (list tag)))))
;; 	(org-set-tags my/org-tags-for-this-entry)))))

;; (defun gtd-skip-all-siblings-but-first ()
;;   "Skip all but the first non-done entry."
;;   (let (should-skip-entry)
;;     (unless (org-current-is-todo)
;;       (setq should-skip-entry t))
;;     (save-excursion
;;       (while (and (not should-skip-entry) (org-goto-sibling t))
;;         (when (org-current-is-todo)
;;           (setq should-skip-entry t))))
;;     (when should-skip-entry
;;       (or (outline-next-heading)
;;           (goto-char (point-max))))))

;; (defun org-current-is-todo ()
;;   (string= "TODO" (org-get-todo-state)))

;; (defun gtd ()
;;   "Visit the gtd inbox"
;;   (interactive)
;;   (find-file "~/Drives/Sync/Agenda/gtd-inbox.org"))

;; (defun gtd-gtd ()
;;   "Visit the gtd work file"
;;   (interactive)
;;   (find-file "~/Drives/Sync/Agenda/gtd-gtd.org"))

;;; C-c C-x C-a archive done projects (org-archive-subtree-default)

;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:
