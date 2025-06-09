;;; 08hideshow.el --- Emacs Initialization -- Hide-show

;;; Commentary:
;;
;; Configuration Settings for hide-show, a lot of it from:
;; https://www.emacswiki.org/emacs/HideShow
;;

;;; Code:

(use-package hideshow
  :config
  (defun toggle-selective-display (column)
      (interactive "P")
      (set-selective-display
       (or column
           (unless selective-display
             (1+ (current-column))))))

  (defun toggle-hiding (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              (hs-show-all))
        (toggle-selective-display column)))

  (defun hs-hide-leafs-recursive (minp maxp)
    "Hide blocks below point that do not contain further blocks in region (MINP MAXP)."
    (when (hs-find-block-beginning)
      (setq minp (1+ (point)))
      (funcall hs-forward-sexp-func 1)
        (setq maxp (1- (point))))
    (unless hs-allow-nesting
      (hs-discard-overlays minp maxp))
    (goto-char minp)
    (let ((leaf t))
      (while (progn
               (forward-comment (buffer-size))
               (and (< (point) maxp)
                    (re-search-forward hs-block-start-regexp maxp t)))
        (setq pos (match-beginning hs-block-start-mdata-select))
        (if (hs-hide-leafs-recursive minp maxp)
            (save-excursion
              (goto-char pos)
              (hs-hide-block-at-point t)))
        (setq leaf nil))
      (goto-char maxp)
      leaf))

  (defun hs-hide-leafs ()
    "Hide all blocks in the buffer that do not contain subordinate
    blocks.  The hook `hs-hide-hook' is run; see `run-hooks'."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (message "Hiding blocks ...")
       (save-excursion
         (goto-char (point-min))
         (hs-hide-leafs-recursive (point-min) (point-max)))
       (message "Hiding blocks ... done"))
     (run-hooks 'hs-hide-hook)))

  (let ((custom-keyvals
	 '(("C-M-<left>"    . hs-hide-block)
	   ("C-M-<right>"   . hs-show-block)
	   ("C-M-S-<left>"  . hs-hide-all)
	   ("C-M-S-<right>" . hs-show-all)
	   ("s-<down>"      . hs-hide-level)
	   ("C-s-<down>"    . hs-hide-level-recursive)
	   ("C-+"	    . toggle-hiding)
	   ("C-="	    . toggle-selective-display)
	   ("C-M-s-l"       . hs-hide-leafs)))
	(default-keybind
	 '(("-- Hide/Show Minor mode hooks --" . "")
	   ("C-c @ C-M-h" . "To fold it all (hs-hide-all)")
	   ("C-c @ C-h"	  . "hide current block (hs-hide-block)")
	   ("C-c @ c-l"	  . "hide all blocls ARG levels below this block (hs-hide-level)")
	   ("C-C @ C-s"	  . "show current block (hs-show-block)")
	   ("C-c @ C-a"	  . "show everything"))))
    (cl-loop for (key . cmd) in custom-keyvals
	     do (keymap-global-set key cmd))
    (key-bindings-more (append default-keybind custom-keyvals)))

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

(provide '08hideshow)

;;; 08hideshow.el ends here
