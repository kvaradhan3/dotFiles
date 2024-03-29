;;; 60cloud.el --- Cloud Manipulation Packages

;;; Commentary:
;;

;;; Code:

(use-package kubernetes)
(use-package kubernetes-helm)

(use-package docker
  :ensure t
  :bind ("C-c r" . docker))

(provide '60cloud)

;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:

;;; 60cloud.el ends here
