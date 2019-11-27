;;; Package --- mryall's .emacs

;;; Last Modified: 20190106

;;; Commentary:
;;
;; Bootstrap the required libraries to enable a literate Emacs configuration
;;

;;; Code:

;; Bootstrap 'package', 'straight', 'use-package' and 'org-mode'
(require 'package)
(setq package-archives
       (append package-archives
              '(("melpa" . "https://melpa.org/packages/")
		("org" . "https://orgmode.org/elpa/"))))
(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(eval-when-compile
  (setq straight-use-package-by-default t)
  (straight-use-package 'use-package))

(unless (package-installed-p 'org-plus-contrib)
  (straight-use-package 'org-plus-contrib))

;; Load Org-Babel defined config
(org-babel-load-file (concat user-emacs-directory "emacs.org"))

;;; .emacs ends here
