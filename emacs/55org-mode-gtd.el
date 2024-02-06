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
  (setq org-gtd-update-ack "3.0.0"
	org-gtd-directory  (expand-file-name "~/NOTES/TASKS")
	org-reverse-note-order   t
        org-edna-use-inheritance t)

  :bind
  (("C-c d c"		. org-gtd-capture)
   ("C-c d e"		. org-gtd-engage)
   ("C-c d p"		. org-gtd-process-inbox)

   ("C-c c"         . org-gtd-organize))

  :config
  (org-edna-mode)
  (org-gtd-mode t))

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

;; ;;; https://github.com/progfolio/doct
;; (use-package doct)

;; (use-package org-capture
;;   :straight nil
;;   :after org
;;   :bind
;;    ("C-c c"       . org-capture)
;;    ("C-c n"       . (lambda ()
;;                       (interactive)
;;                       (org-capture nil "n")))
;;   :config
;;   (setq org-capture-templates
;; 	(append org-capture-templates
;; 		(doct '(
;; 			("(H) Todo [inbox]"
;; 			 :keys "t"
;; 			 :type entry
;; 			 :file "~/Drives/Sync/Agenda/gtd-inbox.org"
;; 			 :regexp "Tasks"
;; 			 :clock t
;; 			 :template ("* TODO %^{Task}"
;; 				    "  SCHEDULED: %^{Due Date}T"
;;                                     "  BLEAH"
;; 				    "  %?")
;; 			 )
;; 			("(H) Tickler"
;; 			 :keys "T"
;; 			 :type entry
;; 			 :file "~/Drives/Sync/Agenda/gtd-ticker.org"
;; 			 :regexp "Tickler"
;; 			 :template ("* %^{Reminder}i %^G"
;; 				    "  SCHEDULED: %^{Next Due Date}T"
;; 				    "  ADDED: %U"
;; 				    "  "
;; 				    "  %?")
;; 			 )
;; 			("(H) Home Project List"
;; 			 :keys "h"
;; 			 :type entry
;; 			 :file "~/Drives/Sync/Agenda/gtd-inbox.org"
;; 			 :regexp "Projects"
;; 			 :template ("* Project: %^{Task} %^G"
;; 				    "  "
;; 				    "  %?")
;; 			 )
;; 			))))
;;   )
;; ;; (require 'org-capture)

;; ;; (use-package helm-org-rifle
;; ;;  :bind
;; ;;  ("M-g r r"    . helm-org-rifle)               ;; Show results from all open Org buffers
;; ;;  ("M-g r R"    . helm-org-rifle-occur)
;; ;;  ("M-g r a"    . helm-org-rifle-agenda-files)  ;; Show results from Org agenda files
;; ;;  ("M-g r A"    . helm-org-rifle-occur-agenda-files)
;; ;;  )
;; ;;(require 'helm-org-rifle)

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
