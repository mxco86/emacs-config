;;; package --- mryall's .emacs

;;; Last Modified: 20190106

;;; Commentary:

;;; Code:

;; Bootstrap 'package', 'use-package' and 'org-mode'
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      (append package-archives
              '(("melpa" . "https://melpa.milkbox.net/packages/")
		("org" . "https://orgmode.org/elpa/"))))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(unless (package-installed-p 'org-plus-contrib)
  (package-refresh-contents)
  (package-install 'org-plus-contrib))

;; Load Org-Babel defined config
(org-babel-load-file (concat user-emacs-directory "emacs.org"))

;;; .emacs ends here
