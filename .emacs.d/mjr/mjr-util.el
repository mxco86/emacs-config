;; some utilities

;;;; load mode configs
(defun mjr/load-mode-cfg ()
  (let ((mode-conf-dir "~/.emacs.d/mjr/mode.d/"))
    (mapcar '(lambda (conf-file)
               (load-file (concat mode-conf-dir conf-file)))
            (directory-files mode-conf-dir nil "[^\.\.?]"))))

;; file paths in the buffer
(defun mjr/insert-path (file)
  "insert file"
  (interactive "FPath: ")
  (insert (expand-file-name file)))

;; google search
(defun mjr/google-region (&optional flags)
  "Google the selected region"
  (interactive)
  (let ((query (buffer-substring (region-beginning) (region-end))))
    (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))

(global-set-key "\C-cg" 'mjr-google-region)

;; font sizing
(defun mjr/init-font-size (frame font value)
  (set-frame-parameter frame
                       'font
                       (concat font
                               "-"
                               (number-to-string value))))

(defun mjr/font-size (font value)
  (interactive "sFont Name:
nText Size: ")
  (mjr/init-font-size (selected-frame) font value))

(defun mjr/set-font ()
  (interactive)
  (mjr/init-font-size (selected-frame) "DejaVu Sans Mono" 16))

;; system identification
(defun mjr/system-type-is (system)
  "Return true if OS string matches"
  (string-equal system-type system))

(defun mjr/system-domain-is (domain)
  "Return true if system domain matches argument string"
  (memq t (mapcar (lambda (x) (string-equal domain x))
                    (split-string (system-name) "\\."))))

;; org mode/wiki function
(defun mjr/org-tbl-to-conf (start end)
  (interactive "r")
  (save-restriction
    (narrow-to-region start end)
    (goto-char (point-min))
    (while (replace-regexp "--+" ""))
    (goto-char (point-min))
    (while (replace-regexp "+" "|"))))

(provide 'mjr-util)
