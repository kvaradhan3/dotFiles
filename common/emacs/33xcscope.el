;; 33c.el --- Emacs Initialization -- C bindings
;;; Commentary:
;;

;;; Code:

(use-package xcscope
  :init
  (setq cscope-truncate-lines t
        cscope-use-relative-paths t
        cscope-index-recursively  t
        cscope-display-times t
        cscope-do-not-update-database t)

  ;; ;;; on mac, C-f4, C-f6, C-f10 M-f10 dont work
  ;; ;;; on mac, turn on keyboard shortcuts
  (setq my-cscope-custom-bindings
	'(("-- CScope Custom bindings --" . "")
	  ("C-<f1>"    . cscope-find-global-definition-no-prompting)
	  ("M-<f1>"    . cscope-find-global-definition)
	  ("C-<f2>"    . cscope-pop-mark)
	  ("M-<f2>"    . cscope-pop-mark)
	  ("C-<f3>"    . cscope-find-this-symbol)
	  ("M-<f3>"    . cscope-find-this-symbol)
	  ("C-<f12>"   . cscope-display-buffer-toggle)
	  ("M-<f12>"   . cscope-display-buffer-toggle)
	  ("C-<f8>"    . cscope-history-backward-line)
	  ("C-<f9>"    . cscope-history-forward-line)
	  ("C-<f5>"    . cscope-set-initial-directory)
	  ("M-<F5>"    . cscope-unset-initial-directory)))
  (key-bindings-more my-cscope-custom-bindings)
  (cl-loop for (key . val) in (cdr my-cscope-custom-bindings)
           do (bind-key key val))

  (key-bindings-more
	'(("-- CSCope Default Bindings --" . "")
	  ("C-c s ="	.	cscope-find-assignments-to-this-symbol)
	  ("C-c s A"	.	cscope-unset-initial-directory)
	  ("C-c s B"	.	cscope-display-buffer-toggle)
	  ("C-c s C"	.	cscope-find-called-functions)
	  ("C-c s D"	.	cscope-dired-directory)
	  ("C-c s E"	.	cscope-edit-list-of-files-to-index)
	  ("C-c s G"	.	cscope-find-global-definition-no-prompting)
	  ("C-c s I"	.	cscope-index-files)
	  ("C-c s L"	.	cscope-create-list-of-files-to-index)
	  ("C-c s N"	.	cscope-history-forward-file-current-result)
	  ("C-c s P"	.	cscope-history-backward-file-current-result)
	  ("C-c s S"	.	cscope-tell-user-about-directory)
	  ("C-c s T"	.	cscope-tell-user-about-directory)
	  ("C-c s W"	.	cscope-tell-user-about-directory)
	  ("C-c s a"	.	cscope-set-initial-directory)
	  ("C-c s b"	.	cscope-display-buffer)
	  ("C-c s c"	.	cscope-find-functions-calling-this-function)
	  ("C-c s d"	.	cscope-find-global-definition)
	  ("C-c s e"	.	cscope-find-egrep-pattern)
	  ("C-c s f"	.	cscope-find-this-file)
	  ("C-c s g"	.	cscope-find-global-definition)
	  ("C-c s i"	.	cscope-find-files-including-file)
	  ("C-c s n"	.	cscope-history-forward-line-current-result)
	  ("C-c s p"	.	cscope-history-backward-line-current-result)
	  ("C-c s s"	.	cscope-find-this-symbol)
	  ("C-c s t"	.	cscope-find-this-text-string)
	  ("C-c s u"	.	cscope-pop-mark)))


  :config
  (setq cscope-do-not-update-database t)
  (with-eval-after-load "cscope-minor-mode"
    (let ((prefix-map (keymap-lookup cscope-minor-mode-keymap "C-c s")))
      (define-key cscope-minor-mode-keymap (kbd "C-c s") nil)
      (define-key cscope-minor-mode-keymap (kbd "C-<f11>") prefix-map)
      ))
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

(provide '31xscope)

;;; 31xcscope.el ends here
