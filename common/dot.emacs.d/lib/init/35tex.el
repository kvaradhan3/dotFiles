;; 35tex.el --- Emacs Initialization -- TeX/LaTeX bindings
;;; Commentary:
;;

;;; Code:

(use-package tex-mode
  :mode
  ("\\.tex\\'" . latex-mode)

  :bind
  (
   :map latex-mode-map
        ("M-<delete>" . tex-remove-macro)
        ("C-c C-r" . reftex-query-replace-document)
        ("C-c C-g" . reftex-grep-document)
	)

  :init
  (setq TeX-auto-save t
        TeX-save-query nil       ; don't prompt for saving the .tex file
        TeX-parse-self t
        TeX-show-compilation t
        LaTeX-babel-hyphen nil ; Disable language-specific hyphen insertion.
        ;; `"` expands into csquotes macros (for this to work, babel pkg must be loaded after csquotes pkg).
        TeX-file-extensions '("Rnw" "rnw" "Snw" "snw" "tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx"))

  :config
  (add-hook 'latex-mode-hook
	    (lambda ()
	      (setq tex-compile-commands
		    (append tex-compile-commands
			    '(("evince %r.pdf" "%r.pdf"))))))

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

(provide '35tex)
