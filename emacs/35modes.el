
(use-package yaml-mode
  :hook (yaml-mode . (lambda () (whitespace-turn-on)))
  )

(use-package json-mode)

(use-package jinja2-mode
  :mode         ("\\.tpl\\'" "\\.jinja\\'" "\\.j2\\'" "\\.jinja2\\'")
  :hook (jinja2-mode . (lambda () (whitespace-turn-on)))
  )

(use-package rust-mode)

;;;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:
