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
(defvar fonts-default-font 'monaco-14)
(defvar fonts-current-font nil)
(defvar fonts-all-fonts-alist
  '((fixed   . "fixed")
    (clean   . "-Schumacher-Clean-Medium-R-Normal--12-120-75-75-C-60-ISO8859-1")
    (bold1   . "lucidasanstypewriter-bold-14")
    (source-code-14     . (fonts-get-font-name "source code pro"  14))
    (source-sans-14     . (fonts-get-font-name "source sans pro"  14))
    (source-serif-14    . (fonts-get-font-name "source serif pro" 14))
    (inconsolata-12     . (fonts-get-font-name "inconsolata"      12))
    (inconsolate-large  . (fonts-get-font-name "inconsolata"      18))
    (menlo-14           . (fonts-get-font-name "menlo"            14))
    (apple-menlo-14     . (fonts-get-font-name "Menlo"            14))
    (monaco-14          . (fonts-get-font-name "Monaco"           14))
    (sfmono-14          . (fonts-get-font-name "SF Mono"          14))
    (menlo-16           . (fonts-get-font-name "menlo"            16))
    (apple-menlo-16     . (fonts-get-font-name "Menlo"            16))
    (monaco-16          . (fonts-get-font-name "Monaco"           16))
    (sfmono-18          . (fonts-get-font-name "SF Mono"          18))
    ))
(defvar fonts-expansion
  '((source-code	. "Source Code Pro")
    (source-sans	. "Source Sans Pro")
    (source-serif	. "Source Serif Pro")
    ("sfmono"		. "SF Mono")
    ("monaco"		. "Monaco")
    ("menlo"		. "Menlo")
    ))

;; 
;; Internal Functions
;; 

(defun fonts-set-font-internal (font-name)
  "Set this frame's font to FONT-NAME, and add to default frame alist."

  (if (not font-name)
      (setq font-name fonts-default-font))
  (let* ((font-name-internal-guess
	  (cdr (assq font-name fonts-all-fonts-alist)))
	 font-name-internal)
    (setq font-name-internal
	  (cond
	   ((not font-name-internal-guess)
	    (let* ((tokens (split-string (symbol-name font-name) "-"))
		   (fp (car (last tokens)))
		   (fn (butlast tokens)))
	      (if (or (not fp) (not fn))
		  (error "Invalid font spec %s" font-name))
	      (setq fn (mapconcat 'identity fn "-"))
	      (if (assq fn fonts-expansion)
		  (setq fn (assq fn fonts-expansion)))
	      (fonts-get-font-name fn (string-to-number fp))))
	   ((listp font-name-internal-guess)
	    (eval font-name-internal-guess))
	   (t font-name-internal-guess)))

    (message "Set Font to %s [%s]" font-name font-name-internal)
    (set-frame-font font-name-internal)
    (setq default-frame-alist (assq-delete-all 'font default-frame-alist))
    (add-to-list 'default-frame-alist (cons 'font font-name-internal))
    (setq fonts-current-font font-name)))

(defun fonts-get-font-name (font-name &optional point)
  "Construct font-name using FONT-NAME and (optional) POINT."
  (if point
      (format "-*-%s-*-*-*-*-%d-*-*-*-*-*-*-*" font-name point)
    (format "-*-%s-*-*-*-*-*-*-*-*-*-*-*-*" font-name)
    ))

(defun fonts-list-fonts-internal ()
   "Return a formatted string of all known fonts."
   (concat (format "default font:\t%s\n" fonts-default-font)
	   (format "current font:\t%s\n" fonts-current-font)
	   "---\n"
	   (string-join
	    (mapcar (lambda (font)
		      (let ((font-name (car font))
			    (font-defn (cdr font)))
			(format "%-16s\t%s"
				font-name
				(if (listp font-defn)
				    (eval font-defn)
				  font-defn))))
		    fonts-all-fonts-alist)
	    "\n")))

;; 
;; Interactive functions
;; 

;;;###autoload
(defun fonts-set-font (&optional font-name)
  "* Set this frame's font to FONT-NAME, use `win:fonts:default' if not given."
  (interactive "Sfont-name? ")
  ;;; TODO interactive code completion hints on possible symbols
  (fonts-set-font-internal font-name))

;;;###autoload
(defun fonts-list-fonts ()
  "* List the set of available fonts for me."
  (interactive)
  (display-message-or-buffer (fonts-list-fonts-internal)))

(provide 'fonts)
;;; fonts.el ends here
