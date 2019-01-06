; -*- mode: Emacs-Lisp;-*-

;; emacs startup and general config

;; be quiet
;;(setq inhibit-startup-message  t)

;;;; clear the furniture
;;(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;;(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; (if (mjr/system-type-is "gnu/linux")
;;     (progn
;;       (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))))

;;;; custom
;;(setq custom-file "~/.emacs-custom.el")
;;(load custom-file 'noerror)

;;;; modes
(show-paren-mode t)
(setq line-number-mode t)
(setq column-number-mode t)
(global-font-lock-mode t)
(mouse-avoidance-mode 'exile)

;;;; buffer behaviour
(setq-default fill-column 78)
(setq-default truncate-lines t)
(setq-default indent-tabs-mode nil)
(setq-default ring-bell-function 'ignore)
(setq-default transient-mark-mode t)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(setq next-line-add-newlines t)

;;;; frame size
;; (if (mjr/system-type-is "darwin")
;;     (set-frame-size (car (visible-frame-list))
;;                     (/ (x-display-pixel-width) 9)
;;                     (/ (x-display-pixel-height) 18)))

;;;; executable path
;; (if (mjr/system-type-is "darwin")
;;     (progn
;;       (add-to-list 'exec-path "/usr/local/git/bin")
;;       (add-to-list 'exec-path "/usr/local/bin")))

;;;; disabled commands
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;;;; function aliases
(fset 'yes-or-no-p 'y-or-n-p)

;;;; lambda symbol
;;(require 'pretty-lambdada)
;;(pretty-lambda-for-modes)

;;;; Delete trailing whitespace
(add-to-list 'write-file-functions
             'delete-trailing-whitespace)

;;;; font
(defun mjr/dejavu ()
  "Normal font family and size"
  (interactive)
  (mjr/font-size "DejaVu Sans Mono" 10))

(global-set-key "\C-c\C-ff" 'mjr/dejavu)

(mjr/set-font)

(provide 'mjr-setup)
