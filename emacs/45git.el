(use-package magit
  )
(require 'magit)

(use-package forge
  :after magit
  )

(use-package code-review
  :config
  (add-hook 'code-review-mode-hook #'emojify-mode)
  (setq code-review-fill-column 80
	code-review-new-buffer-window-strategy #'switch-to-buffer
	code-review-download-dir "/tmp/code-review/")
  (setq code-review-auth-login-marker 'forge)

  ;;; Key Bindings
  ;;; RET 	hunk					Add Comment
  ;;; RET 	comment					Add Reply
  ;;; RET 	local comment (not sent to forge yet) 	Edit local comment
  ;;; C-c C-k 	local comment			 	Delete local comment
  ;;; C-c C-k 	Comment Buffer			 	Cancel your local comment
  ;;; C-c C-c 	Comment Buffer				Register your local comment
  ;;; C-c C-n 	comment				 	Promote to new issue
  ;;; C-c C-r 	comment					Add Reaction
  ;;; C-c C-r 	pr description			 	Add Reaction
  ;;; RET 	reaction (on emoji symbol)	 	Endorse or Remove Reaction
  ;;; RET 	Request Reviewer		 	Request reviewer at point

  :bind-keymap (("C-c C-n"	. code-review-comment-jump-next)
		("C-c C-p"	. code-review-comment-jump-previous))
  :bind (
	 :map forge-topic-mode-map
	      ("C-c r"	. code-review-forge-pr-at-point))
  )

  ;;; Binding suggestions
  ;;; If you are not an Evil user you can set the letter k,
  ;;; for example, to delete a local comment or feedback at point.
  ;;;
  ;;; (define-key code-review-feedback-section-map (kbd "k")
  ;;;   'code-review-section-delete-comment)
  ;;; (define-key code-review-local-comment-section-map (kbd "k")
  ;;;   'code-review-section-delete-comment)
  ;;; (define-key code-review-reply-comment-section-map (kbd "k")
  ;;;   'code-review-section-delete-comment)
  )
