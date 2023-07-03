;;; 20helm.el --- Emacs Initialisation -- Helm Components

;;; Commentary:
;;
;; Routines for configuring my helm utilization
;;

;;; Code:

(use-package helm
  :bind (("M-x"		. helm-M-x)
	 ("C-x C-f"	. helm-find-files))
  :config
  (require 'helm-config)
  (helm-mode 1)
  )

(use-package helm-elisp
  :straight nil
  :bind (("C-h a"	. helm-apropos))
  :config
  (setq helm-apropos-fuzzy-match    t
        helm-apropos-show-short-doc t))

(use-package helm-buffers
  :straight nil
  :config
  (setq helm-buffers-favorite-modes
        (append helm-buffers-favorite-modes '(picture-mode artist-mode))
        helm-buffers-fuzzy-matching       t
        helm-buffer-skip-remote-checking  t
        helm-buffers-maybe-switch-to-tab  t
        helm-mini-default-sources '(helm-source-buffers-list
                                    helm-source-buffer-not-found)
        helm-boring-buffer-regexp-list
        '("\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf"
          "\\`\\*Messages" "\\`\\*Magit" "\\`\\*git-gutter" "\\`\\*Help"))

  (define-key helm-buffer-map (kbd "C-d") 'helm-buffer-run-kill-persistent))

(use-package helm-occur
  :straight nil
  :config
  (setq helm-occur-keep-closest-position t)
  (add-hook 'helm-occur-mode-hook 'hl-line-mode)
  (define-key helm-occur-map (kbd "C-M-a") 'helm/occur-which-func))

(use-package helm-system-packages)

(use-package helm-ls-git
  :bind (("C-x C-d"	. helm-browse-project)
	 ("C-x r p"	. helm-projects-history))
  :custom
  (helm-ls-git-fuzzy-match              t)
  ;; (helm-ls-git-status-command        'magit-status-setup-buffer)
  (helm-ls-git-delete-branch-on-remote  t))
;; (require 'helm-ls-git)
;; after magit


(provide '20helm)

;;; 20helm.el ends here
