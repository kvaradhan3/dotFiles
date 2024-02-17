;;; 05preferences.el --- Emacs Initialization -- General Initialization

;;; Commentary:
;;
;; Configuration Settings that are applicable to the frame and init itself.
;;

;;; Code:

(setq inhibit-startup-message         t
      debug-on-error                  t)

(if window-system
    (progn
      (scroll-bar-mode			nil)
      (menu-bar-mode			-1)
      (tool-bar-mode			-1)
      (mouse-avoidance-mode		'exile)
      (mouse-wheel-mode			t)
      (global-font-lock-mode		t)
      (transient-mark-mode		t)
      (delete-selection-mode		nil)
      (show-paren-mode			t)
      (column-number-mode		1)
      (global-hl-line-mode		1)
      (global-visual-line-mode		1)
      (global-display-line-numbers-mode 1)
      (indent-tabs-mode			nil)
      ))

(use-package column-enforce-mode
  :config
  (setq column-enforce-column 70)
  (global-column-enforce-mode t))

(use-package whitespace
  :config
  (setq whitespace-style
        '(face                           ; viz via faces
          trailing                       ; trailing blanks
          tabs                           ; anyplace?
          newline
          missing-newline-at-eof
          empty                          ; @start/end of buffer
          indentation                    ; @start-of-line >tabspace
          ))
  (global-whitespace-mode t)
  )

(defun make-local-hook (f)
  "Dummy because of ws-trim's wws-trimmode usage"
  nil)

(use-package ws-trim
  :config
  (setq-default ws-trim-level 1)
  (global-ws-trim-mode t)
  )

;;;
;;; minibuffer configurations
;;;
(use-package minibuf-eldef
  :config
  (minibuffer-electric-default-mode t)

  :custom
  (minibuffer-eldef-shorten-default t)
  )

;;;
;;; modeline
;;;
(let ((display-time-day-and-date t))
  (display-time))

(use-package powerline
  :config
  (powerline-default-theme)
  ;; (powerline-center-theme)
  ;; (powerline-center-evil-theme)
  ;; (powerline-vim-theme)
  ;; (powerline-nano-theme)
  )

(use-package eclipse-theme)
(require 'eclipse-theme)


;;;
;;; file and buffer control
;;;
(add-hook 'before-save-hook 'time-stamp)
(setq require-final-newline             nil
      fill-column                        72
      case-fold-search                  nil
      backup-by-copying-when-linked       t
      backup-by-copying-when-mismatch     t
      enable-local-eval                   t)
(setq-default   case-fold-search        nil)

;;;
;;; windows fonts.
;;;
(use-package fonts
  :straight (:local-repo "~/.emacs.d/lib/fonts"
             :type       nil)
  :config
  (fonts-set-font))

;;;
;;; search
;;;
(use-package swiper
  :bind (("C-s" . 'swiper)
         ("C-r" . 'swiper-backward)))


;;;
;;; mac specifics
;;;
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config
  (let ((exec-path-from-shell-check-startup-files nil))
    (exec-path-from-shell-initialize)))

(use-package ls-lisp
  :straight nil
  :if (memq window-system '(mac ns))
  :config
  (setq ls-lisp-use-insert-directory-program nil))

;;;
;;; I like vertical splits
;;;
(setq split-height-threshold 100
      split-width-threshold  100)

(use-package info
  :mode (("\\.info\\'"            . Info-mode)))


;;;
;;; Keybindings that we care about, as defaults
;;;
(define-key     ctl-x-map               [?\C-m] #'manual-entry)
(define-key     ctl-x-map               [?\C-b] #'electric-buffer-list)

(define-key     esc-map         [?`]            #'shell-command-on-buffer)
(define-key     esc-map         [?%]            #'query-replace-regexp)
(define-key     esc-map         [?\C-^]         #'replace-regexp)
(define-key     esc-map         [?\C-g]         #'goto-line)
;; (define-key  esc-map         [?\C-r]         #'isearch-backward-regexp)

;
;;; Local Variables:
;;; mode: Emacs-Lisp
;;; comment-column: 60
;;; comment-start: "; "
;;; comment-end: ""
;;; backup-by-copying-when-linked: t
;;; file-precious-flag: nil
;;; End:

(provide '05preferences)

;;; 05preferences.el ends here
