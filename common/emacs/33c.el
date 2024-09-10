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

  :bind
  ;;; on mac, C-f4, C-f6, C-f10 M-f10 dont work 
  ;;; on mac, turn on keyboard shortcuts
  (
   ("C-<f1>"  . cscope-find-global-definition)
   ("M-<f1>"  . cscope-find-global-definition-no-prompting)
   ("C-<f2>"  . cscope-find-this-symbol)
   ("C-<f3>"  . cscope-pop-mark)
  
   ("C-<f12>" . cscope-display-buffer-toggle)
   ("M-<f12>" . cscope-display-buffer-toggle)

   ("C-<f8>"  . cscope-history-backward-line)
   ("C-<f9>"  . cscope-history-forward-line)

   ("C-<f5>"  . cscope-set-initial-directory)
   ("M-<f5>"  . cscope-unset-initial-directory)

    ;; Key             Binding
    ;; 
    ;; S-<mouse-3>	cscope-mouse-search-again
    ;; <mouse-3>	cscope-mouse-popup-menu-or-search
    ;; 
    ;; C-c s =			cscope-find-assignments-to-this-symbol
    ;; C-c s A			cscope-unset-initial-directory
    ;; C-c s B			cscope-display-buffer-toggle
    ;; C-c s C			cscope-find-called-functions
    ;; C-c s D			cscope-dired-directory
    ;; C-c s E			cscope-edit-list-of-files-to-index
    ;; C-c s G			cscope-find-global-definition-no-prompting
    ;; C-c s I			cscope-index-files
    ;; C-c s L			cscope-create-list-of-files-to-index
    ;; C-c s N			cscope-history-forward-file-current-result
    ;; C-c s P			cscope-history-backward-file-current-result
    ;; C-c s S .. C-c s T	cscope-tell-user-about-directory
    ;; C-c s W			cscope-tell-user-about-directory
    ;; C-c s a			cscope-set-initial-directory
    ;; C-c s b			cscope-display-buffer
    ;; C-c s c			cscope-find-functions-calling-this-function
    ;; C-c s d			cscope-find-global-definition
    ;; C-c s e			cscope-find-egrep-pattern
    ;; C-c s f			cscope-find-this-file
    ;; C-c s g			cscope-find-global-definition
    ;; C-c s i			cscope-find-files-including-file
    ;; C-c s n			cscope-history-forward-line-current-result
    ;; C-c s p			cscope-history-backward-line-current-result
    ;; C-c s s			cscope-find-this-symbol
    ;; C-c s t			cscope-find-this-text-string
    ;; C-c s u			cscope-pop-mark
  )

  :config
  (setq cscope-do-not-update-database t)
  (with-eval-after-load "cscope-minor-mode"
    (let ((prefix-map (keymap-lookup cscope-minor-mode-keymap "C-c s")))
      (define-key cscope-minor-mode-keymap (kbd "C-c s") nil)
      (define-key cscope-minor-mode-keymap (kbd "C-<f11>") prefix-map)
      ))
)

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
