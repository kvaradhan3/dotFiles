;;; 00init.el --- Initialize straight
;;; Commentary:
;;  Emacs Startup File --- invoked through ~/.emacs
;;; Code:

(setq package-enable-at-startup        nil)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;      (straight-repository-branch "develop")
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package straight
  :custom
  ;; (straight-check-for-modifications nil)
  (straight-use-package-by-default  t))

(use-package sqlite3)

;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/lib/"))

;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:
;;; 00init.el ends here
