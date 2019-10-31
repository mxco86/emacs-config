;; my development tools 

(require 'thingatpt)

(defun xt-grep-find (thing)
  (interactive "sSearch XT Source: ")
  (grep-find (concat "find /home/mryall/nap-src/xt "
                     "-type f -print0 "
                     "| xargs -0 -e grep -n -s \ -F \"" thing "\"")))

(defun xt-grep-find-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (grep-find (concat "find /home/mryall/nap-src/xt "
                       "-type f -print0 "
                       "| xargs -0 -e grep -n -s \ -F \"" thing "\""))))

(defun nap-grep-find-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (grep-find (concat "find /home/mryall/nap-src/webapp "
                       "-type f -print0 "
                       "| xargs -0 -e grep -n -s \ -F \"" thing "\""))))

(defun perl-find-subs ()
  "Find all perl subroutines in the current buffer"
  (interactive)
  (occur "sub .*[\n\s]*{"))

(defun occur-at-point ()
  "occur on the current 'thing'"
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (occur thing)))

;;

(provide 'mjr-dev-tools)