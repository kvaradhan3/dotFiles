;;; 15secrets.el --- Emacs Initialization -- GPG and secrets setup

;;; Commentary:
;; 

;;; Code:
(use-package auth-source
  :config
  (setq auth-sources '("~/.authinfo.gpg"))
  )

; GPG
(use-package epa
  :init
  (setenv "GPG_AGENT_INFO" nil)
  (setf epa-pinentry-mode  'loopback)
  )

(provide '15secrets)

;;; 15secrets.el ends here
