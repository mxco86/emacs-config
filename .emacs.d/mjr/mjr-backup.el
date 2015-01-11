;;; general file back up 

;; create a backup file directory
(defvar backup-dir
  (concat "/tmp/emacs_backups/" (user-login-name) "/"))

(make-directory backup-dir t)

;; put backups in one place
(setq backup-directory-alist (list (cons "." backup-dir))) 

;; put autosaves in one place
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))

(make-directory autosave-dir t)

;; do we need a filename?
(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

;; create a filename for saving backups
(defun make-auto-save-file-name ()
  (concat autosave-dir
 	  (if buffer-file-name
  	      (concat "#"
                      (file-name-nondirectory buffer-file-name) "#")
  	    (expand-file-name (concat "#%" (buffer-name) "#")))))
;;

(provide 'mjr-backup)