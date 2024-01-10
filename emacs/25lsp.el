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

  ;;; lsp-find-references
  ;;; https://emacs-lsp.github.io/lsp-mode/page/main-features/
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
  :config
  ;;; (yas-global-mode 1)
  (add-to-list	'yas-snippet-dirs
		"https://github.com/dominikh/yasnippet-go")
  )

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
