
(use-package yaml-mode
  :hook (yaml-mode . (lambda () (whitespace-turn-on)))
  )

(use-package json-mode)

(use-package jinja2-mode
  :mode         ("\\.tpl\\'" "\\.jinja\\'" "\\.j2\\'" "\\.jinja2\\'")
  :hook (jinja2-mode . (lambda () (whitespace-turn-on)))
  )

(use-package rust-mode)

(use-package origami
  :config
  (define-prefix-command 'origami-mode-map)
  (define-key mode-specific-map (kbd "z") 'origami-mode-map)
  (global-origami-mode)
  :bind (:map origami-mode-map
	      ("o"   . origami-open-node)
	      ("O"   . origami-open-node-recursively)
	      ("c"   . origami-close-node)
	      ("C"   . origami-close-node-recursively)
	      ("C-o" . origami-open-all-nodes)
	      ("C-c" . origami-close-all-nodes)
	      ("t"   . origami-toggle-node)
	      ("T"   . origami-recursively-toggle-node)
	      ("v"   . origami-show-only-node)
	      ("k"   . origami-previous-fold)
	      ("j"   . origami-forward-fold)
	      ("x"   . origami-reset)))

(require 'origami)

;;;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:
