;; Matthew Ryall's .emacs
;;
;; Last Modified: 20150103
;;

;; add lisp directories
(let* ((elisp-dir "~/.emacs.d/site-lisp")
       (mjr-elisp-dir "~/.emacs.d/mjr")
         (default-directory elisp-dir))
  (add-to-list 'load-path elisp-dir)
  (add-to-list 'load-path mjr-elisp-dir)
  (normal-top-level-add-subdirs-to-load-path))

;; yum!
(require 'cl)

;; Package setup
(require 'mjr-package-list)
(require 'mjr-packages)

;; Load basic utilities and config
(require 'mjr-util)
(require 'mjr-setup)
(require 'mjr-keymap)
(require 'mjr-backup)

;; load mode specific configs
(mjr/load-mode-cfg)
