;; (require 'company-lsp)
;; (push 'company-lsp company-backends)

(setq lsp-keymap-prefix "C-c l")

(use-package which-key)

(use-package lsp-mode
  :hook     ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-auto-guess-root              t
        lsp-prefer-flymake               nil
        lsp-enable-semantic-highlighting t
        lsp-enable-completion-at-point	 t
        lsp-completion-provider          :capf)
  (setq lsp-log-io                       t)

  ;; 	GO111MODULE=on go get golang.org/x/tools/gopls
  (add-hook 'go-mode-hook
	    (lambda ()
	      "Setup before-save hooks to formate buffer and add/delete imports."
	      (lsp-deferred)
	      (add-hook 'before-save-hook #'lsp-format-buffer    nil t)
	      (add-hook 'before-save-hook #'lsp-organize-imports nil t))
	    nil t)
  ;;; lsp-find-references
  ;;; https://emacs-lsp.github.io/lsp-mode/page/main-features/
  )

(use-package company
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay            0.0) ;; default is 0.2
  )

;; optional - provides fancier overlays
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable nil
        lsp-ui-doc-enable nil
        lsp-ui-flycheck-enable nil
        lsp-ui-imenu-enable nil
        lsp-ui-sideline-ignore-duplicate t))

(use-package yasnippet
  :hook ((go-mode . yas-minor-mode))
  :config
  ;;; (yas-global-mode 1)
  (add-to-list	'yas-snippet-dirs
		"https://github.com/dominikh/yasnippet-go")
  )

;;; (use-package helm-lsp
;;;   :commands helm-lsp-workspace-symbol)

;;; (use-package lsp-treemacs
;;;    :commands lsp-treemacs-errors-list)

;; (use-package dap-mode)
;; (use-package dap-go)
;; (use-package which-key
  ;; :config
  ;; (which-key-mode))

;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp)

;;;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:
