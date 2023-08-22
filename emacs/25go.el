;;; 25go.el --- Emacs Initialization -- go bindings

;;; Commentary:
;; 
;; https://dr-knz.net/a-tour-of-emacs-as-go-editor.html
;; https://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/https://tleyden.github.io/blog/2014/05/22/configure-emacs-as-a-go-editor-from-scratch/
;;

;;; Code:

(use-package go-mode
  ;;
  ;; GO111MODULE=off go get golang.org/x/tools/cmd/...
  ;; GO111MODULE=off go get github.com/rogpeppe/godef
  ;;
  :bind (("M-."	. 'godef-jump)
	 ("M-," . 'pop-tag-mark)
	 ("C-c C-a"	. 'go-import-add)
	 ("C-c C-r"	. 'go-remove-unused-imports)
	 ("C-c C-g"	. 'go-goto-imports)
	 ("C-c ?"	. 'godoc)
	 ("C-c C-d"	. 'godef-describe))
	 ;; ("M-g i a"	. 'go-import-add)
	 ;; ("M-g i r"	. 'go-remove-unused-imports)
	 ;; ("M-g i g"	. 'go-goto-imports))
        ;; C-M-a beginning of defun
        ;; C-M-e end of defun
        ;; C-M-h mark defun
        ;; C-x n w narrow to defun
        ;; C-c C-d godef-describe
        ;; C-c C-j godef jump

	;; playground:
        ;; go-play-buffer	send to playground, store url in kill ring
	;; go-play-region
        ;; go-download-play

  :config
  (setq tab-width        4
	indent-tabs-mode nil
	truncate-lines   t)
  ;;
  ;; GO111MODULE=off go get golang.org/x/tools/cmd/goimports
  ;;
  (setq gofmt-command "goimports")

  (add-hook 'before-save-hook	#'gofmt-before-save	nil t)

  ;; 	GO111MODULE=on go get golang.org/x/tools/gopls
  )

(use-package go-autocomplete
  ;;
  ;; GO111MODULE=off go get -u github.com/nsf/gocode
  ;;
  :config
  (add-hook 'go-mode-hook #'(lambda () (auto-complete-mode 1)))
  )

(use-package go-eldoc
  :after go-mode
  :hook ((go-mode	. go-eldoc-setup))
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

;; (use-package lsp-mode
;;   :after    go-mode
;;   :config
;;   (add-hook 'go-mode-hook #'lsp))

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
