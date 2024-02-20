;;; 25go.el --- Emacs Initialization -- go bindings

;;; Commentary:
;; 
;; https://dr-knz.net/a-tour-of-emacs-as-go-editor.html
;; https://geeksocket.in/posts/emacs-lsp-go/
;; https://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;; https://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;;

;;; Code:

(use-package go-mode
  ;;
  ;; GO111MODULE=off go get golang.org/x/tools/cmd/...
  ;; GO111MODULE=off go get github.com/rogpeppe/godef
  ;;
  :bind (("M-."		. 'godef-jump)
	 ("M-,"		. 'pop-tag-mark)
	 ("C-c C-a"	. 'go-import-add)
	 ("C-c C-r"	. 'go-remove-unused-imports)
	 ("C-c C-g"	. 'go-goto-imports)
	 ("C-c ?"	. 'godoc)
	 ("C-c C-d"	. 'godef-describe))
         ;; C-M-a beginning of defun
         ;; C-M-e end of defun
         ;; C-M-h mark defun
         ;; C-x n w narrow to defun
         ;; C-c C-d godef-describe
         ;; C-c C-j godef jump

         ;; playground:
         ;; go-play-buffer ; send to playground, store url in kill ring
	 ;; go-play-region
         ;; go-download-play

  :init
  (setq tab-width        4
	indent-tabs-mode nil
	truncate-lines   t)
  ;;
  ;; go install golang.org/x/tools/cmd/goimports@latest
  ;; go install golang.org/x/tools/gopls@latest
  ;;
  (setq gofmt-command "goimports"
	gofmt-args    "-s")

  (defun go/set-before-save-hooks nil
    "sets before-save hooks as buffer local to go-mode buffers"
    (add-hook 'before-save-hook	#'lsp-format-buffer		nil t)
    (add-hook 'before-save-hook #'lsp-organize-imports		nil t)
    )

  :hook
  ((go-mode    .    lsp-deferred)
   (go-mode    .    go/set-before-save-hooks))
)

(use-package go-autocomplete
  ;;
  ;; GO111MODULE=off go get -u github.com/nsf/gocode
  ;;
  :init
  (defun go/enable-auto-complete nil (auto-complete-mode t))
  :hook
  ((go-mode    .    go/enable-auto-complete))
  )

(use-package go-eldoc
  :hook ((go-mode    . go-eldoc-setup))
  :config
  (set-face-attribute 'eldoc-highlight-function-argument nil
                      :underline t :foreground "green"
                      :weight 'bold)
  )

;; (use-package go-flymake)
;; (use-package go-flycheck)
;; (use-package go-errcheck)

;; (use-package flycheck
;;   ;;
;;   ;; https://www.flycheck.org/en/latest/user/quickstart.html
;;   ;;
;;   :bind (("C-c ! n"	. flycheck-next-error)
;; 	 ("C-c ! p"	. flycheck-previous-error)
;; 	 ("C-c ! l"	. flycheck-list-errors))
;;   :config
;;   (global-flycheck-mode 1))

;; (use-package go-errcheck
;;   ;;
;;   ;; https://github.com/dominikh/go-errcheck.el
;;   ;;
;;   )

(use-package company
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay            0.0) ;; default is 0.2
  )

;;
;; go debugging
;;

;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:

(provide '25go)

;;; 25go.el ends here
