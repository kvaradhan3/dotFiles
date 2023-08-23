
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(canlock-password "fbce9060bb6f2afd7e7fc4601bb20f3928bd0d39")
;; '(package-selected-packages
;;   (quote
;;    (helm-org-rifle helm-org helm popwin async google org json-mode yaml-mode twittering-mode xcscope filladapt auto-org-md markdown-mode markdown-mode+ markdown-preview-mode markdown-toc exec-path-from-shell go-autocomplete flymake-go flycheck go-mode magit protobuf-mode)))
 '(safe-local-variable-values
   (quote
    ((prompt-to-byte-compile)
     (file-precious-flag)
     (backup-by-copying-when-linked . t)
     (backup-by-copying . t)))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "~/.emacs-rc-dir")
(dolist (curFile (directory-files (expand-file-name emacs-rc-dir)
                                  t "^[0-9][0-9].*.el$"))
  (load-file curFile))

(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying: t
;;; file-precious-flag: nil
;;; End:
