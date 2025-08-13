;;; 03keybinds.el --- Emacs Initialization -- Help for COmmon Key Bindings

;;; Commentary:
;;
;; A chaet sheet for key bindings that I can never remember...
;;

;;; Code:

(setq key-bindings-help-codes '())
(defun key-bindings-help nil
  "Dump emacs key bindings useful for me that I can never remember"
  (interactive)
  (with-temp-buffer-window "*Key-Bindings Cheat Sheet *"
      '((display-buffer-pop-up-frame)) nil
    (let ((cch-headings '(("Key" . "Binding")
			  ("-------" . "------------------------")))
	  (format-str   "%-10s\t\t%s\n"))
      (princ (cl-loop for (h1 . h2) in cch-headings
		      concat (format format-str h1 h2)))
      (cl-loop for bindings in key-bindings-help-codes
	       do (princ (format "%s\n"
				 (cl-loop for (key . fn) in bindings
					  concat (format format-str
							 key fn))))))))

(defun key-bindings-more (arg-alist)
  "Add another ARG-ALIST to list of help codes for me"
  (setq key-bindings-help-codes
 	(cons arg-alist key-bindings-help-codes)))

;;;
;;; Keybindings that we care about, as defaults
;;;
(define-key     ctl-x-map               [?\C-m] #'manual-entry)
(define-key     ctl-x-map               [?\C-b] #'electric-buffer-list)

(define-key     esc-map         [?`]            #'shell-command-on-buffer)
(define-key     esc-map         [?%]            #'query-replace-regexp)
(define-key     esc-map         [?\C-^]         #'replace-regexp)
(define-key     esc-map         [?\C-g]         #'goto-line)
;; (define-key  esc-map         [?\C-r]         #'isearch-backward-regexp)

;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:

(provide '03keybinds)

;;; 03keybinds.el ends here
