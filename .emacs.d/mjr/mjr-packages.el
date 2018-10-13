;; Package setup

(require 'cask "/usr/local/share/emacs/site-lisp/cask/cask.el")
(cask-initialize)

(require 'pallet)
(pallet-mode t)

;; Package install
(require 'package)
(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ;;("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("org" . "http://orgmode.org/elpa/")))

;; Method to check if all packages are installed
(defun packages-installed-p ()
  (loop for p in required-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

;; If not all packages are installed, check one by one and install the missing
;; ones.
;; (unless (packages-installed-p)
;;   ; check for new packages (package versions)
;;   (message "%s" "Emacs is now refreshing its package database...")
;;   (package-refresh-contents)
;;   (message "%s" " done.")
;;   ; install the missing packages
;;   (dolist (p required-packages)
;;     (when (not (package-installed-p p))
;;       (package-install p))))
;; ;;

(provide 'mjr-packages)
