;;; fonts.el --- Library to set specific fonts in windows
;;
;; Filename: fonts.el
;; Description:
;; Author: Kannan Varadhan <kannan@ieee.org>
;; Maintainer:
;; Created: Sun Jul 31 02:29:52 PDT 2022
;; Version: 0.0.1
;; Package-Requires: ()
;; Keywords:
;; Compatibility: >= Emacs 28.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;  Allow to choose from a liste of tested fonts.
;;  Automatically set to a well known default, if necessary.
;;
;;  To enable: M-x fonts-set-font
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;;
;; Variables
;;
(defvar fonts-default-font 'monaco@14)
(defvar fonts-current-font nil)
(defvar fonts-default-size "*")

(defvar fonts-all-abbrev-alist
  '((fixed		. "fixed")
    (clean		. "Schumacher-Clean-Medium")
    (adwaita		. "Adwaita Mono")
    (anonymouspro	. "Anonymous Pro")
    (inconsolata	. "Inconsolata Nerd Font Mono")
    (lucida		. "Lucida Sans Typewriter-Seml Condensed")
    (menlo		. "Meslo LG L")
    (notosans		. "Noto Sans Mono")
    (noto		. "Noto Sans Mono")
    (source-code	. "source code pro")
    (source-sans	. "source sans pro")
    (source-serif	. "source serif pro")
    (apple-menlo	. "Menlo")
    (monaco		. "Monaco")
    (sfmono		. "SF Mono")
    (sauce		. "SauceCodePro NFM Light")
    (hacknerd		. "Hack Nerd Font Mono")))

;;
;; Internal Functions
;;

(defun fonts-format-font-name (font-name &optional point)
  "Construct font-name using FONT-NAME and (optional) POINT."
  (format "-*-%s-*-*-*-*-%s-*-*-*" font-name (or point fonts-default-size)))

(defun fonts-canonicalize-font-internal (&optional font-name)
  "Given a FONT-NAME, return a canonicalized version that can be set."
  (let* ((font-name-full (or font-name fonts-default-font))
	 (font-tokens (split-string (symbol-name font-name-full) "@"))
	 (font-name (car  font-tokens))
	 (font-size (cadr font-tokens)))
    (fonts-format-font-name (or (cdr (assq (intern font-name) fonts-all-abbrev-alist))
				font-name)
			    (or font-size fonts-default-size))))

(defun fonts-set-font-internal (font-name)
  "Set the frame font to FONT-NAME.  Adjust the frame alist appropriately."
  (let ((font-name-canonical (fonts-canonicalize-font-internal font-name)))
    (set-frame-font font-name-canonical)
    (setq default-frame-alist (assq-delete-all 'font default-frame-alist))
    (add-to-list 'default-frame-alist (cons 'font font-name-canonical))
    (setq fonts-current-font (or font-name font-name-canonical))
    font-name-canonical))

(defun fonts-list-fonts-internal ()
   "Return a formatted string of all known fonts."
   (concat (format "default font name:\t%s\n" fonts-default-font)
	   (format "current font name:\t%s\n" fonts-current-font)
	   (format "default font size:\t%s\n" fonts-default-size)
	   "---\n"
	   (mapconcat (lambda (f)
			(format "%15s\t%-20s\t%s"
				(car f)
				(cdr f)
				(fonts-canonicalize-font-internal (car f))))
		      fonts-all-abbrev-alist "\n")))

;;
;; Interactive functions
;;

;;;###autoload
(defun fonts-set-font (&optional font-name)
  "* Set this frame's font to FONT-NAME, use `win:fonts:default' if not given."
  (interactive "Sfont-name? ")
  ;;; TODO interactive code completion hints on possible symbols
  (message "Set font to %s[%s]" font-name
	   (fonts-set-font-internal font-name)))

;;;###autoload
(defun fonts-list-fonts ()
  "* List the set of available fonts for me."
  (interactive)
  (display-message-or-buffer (fonts-list-fonts-internal)))

(provide 'fonts)
;;; fonts.el ends here
