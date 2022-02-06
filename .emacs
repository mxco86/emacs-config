;;; Package --- mryall's .emacs

;;; Last Modified: 20210106

;;; Commentary:
;;
;; Bootstrap the required libraries to enable a literate Emacs configuration
;;

;;; Code:

;; Bootstrap 'package', 'straight', 'use-package' and 'org-mode'
(require 'package)
(setq package-archives
      (append package-archives
              '(("melpa" . "https://melpa.org/packages/"))))
(package-initialize)

;; Load Org-Babel defined config
(org-babel-load-file (concat user-emacs-directory "emacs.org"))

;;; .emacs ends here
