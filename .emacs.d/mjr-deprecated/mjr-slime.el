;; add this to your ~/.emacs to use clbuild and its slime:
;;

;; possibly controversial as a global default, but shipping a lisp
;; that dies trying to talk to slime is stupid, so:
(set-language-environment "UTF-8")
(setq slime-net-coding-system 'utf-8-unix)

(defun mjr/slime-general-setup ()
  (progn
    (load (expand-file-name "~/quicklisp/slime-helper.el"))    
    (setq inferior-lisp-program "sbcl")
    (setq inhibit-splash-screen t)
    (require 'slime)
    ;;(slime-setup '(slime-fancy))
    ;;(slime-require :swank-listener-hooks)
    (global-set-key "\C-ce" 'slime-selector)))

(defun mjr/slime-linux-setup ()
  (mjr/slime-general-setup))

(defun mjr/slime-darwin-setup ()
  (mjr/slime-general-setup))

;; clojure-mode/swank-clojure
;; (require 'clojure-mode)
;; (require 'swank-clojure-autoloads)
;; (setq swank-clojure-jar-path "~/.clojure/clojure.jar")

;; (add-to-list 'slime-lisp-implementations
;;              `(clojure ,(swank-clojure-cmd)
;;                        :init swank-clojure-init))

;; startup 
(if (mjr/system-type-is "gnu/linux")
    (mjr/slime-linux-setup))

(if (mjr/system-type-is "darwin")
    (mjr/slime-darwin-setup))

;;

(provide 'mjr-slime)
