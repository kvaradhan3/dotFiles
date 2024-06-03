;; 33c.el --- Emacs Initialization -- C bindings
;;; Commentary:
;;

;;; Code:

(use-package xcscope
  :init
  (setq cscope-truncate-lines t
        cscope-use-relative-paths t
        cscope-index-recursively  t
        cscope-do-not-update-database t)

  :config
  (setq cscope-do-not-update-database t))

(use-package cc-mode
  :init
  (setq c-basic-offset   4)
  :config
  (setq c-k&r-plus
        '("k&r"
          (c-comment-only-line-offset . 4)
          (c-cleanup-list .  (scope-operator
                              empty-defun-braces
                              defun-close-semi))))
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-add-style "k&r-plus" c-k&r-plus t)))

  :hook
  ((c-mode-common      .       cscope-setup))
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

(provide '33c)

;;; 25go.el ends here

;;;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:
