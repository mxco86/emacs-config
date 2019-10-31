
(defun mjr/date-convert (date-string)
  "Convert the date from smile's stupid format"
  (let ((date-elems (vconcat (split-string date-string "/")))) 
    (concat (aref date-elems 2) "/"
            (aref date-elems 1) "/"
            (aref date-elems 0))))

(setq smile-regex 
      (concat "\\([0-9]\\{2\\}\/[0-9]\\{2\\}\/[0-9]\\{4\\}\\) "   ;; date
              "	"                                                ;; field separator
              "\\([^	]+\\) "                                  ;; payment reference
              "	"                                                ;; field separator
              "\\(\\( +\\)\\|\\(£[0-9]+.[0-9]+ \\)\\)"           ;; amount
              "	"                                                ;; field separator
              "\\(\\( +\\)\\|\\(£[0-9]+.[0-9]+ \\)\\)"           ;; amount
              ))

(setq sw-regex 
      (concat "\\([0-9]\\{2\\}/[0-9]\\{2\\}/[0-9]\\{4\\}\\)"
              "		"
              "\\([^	]+\\)"
              "	£[0-9]+.[0-9]+"
              "	\\(£[0-9]+.[0-9]+\\)	"
              ))

(setq cht-regex
      "\\([0-9]\\{1,2\\} [a-z]\\{3\\} [0-9]\\{4\\}\\) 	\\([^	]+\\)  \\(\\(+\\)\\|\\(£[0-9]+.[0-9]+\\)\\)")

(defun mjr/statement-item (item-regex line)
  "Create statement data from Smile statement item"
  (string-match item-regex line)
  (let ((date       (match-string 1 line))
        (payref     (match-string 2 line))
        (in-amount  (match-string 3 line))
        (out-amount (match-string 6 line)))
    (values (mjr/date-convert date) payref in-amount out-amount)))

(defun mjr/ledger-item (statement-item)
  "Create a ledger item from a statement item"
  (if (string-match "£.+" (third statement-item))
      (format "%s %s \n    Assets:Joint %s\n    {source}\n\n"
            (first statement-item)
            (second statement-item)
            (third statement-item))
    (format "%s %s \n    {dest}  %s\n    Assets:Joint\n\n"
            (first statement-item)
            (second statement-item)
            (fourth statement-item))))

(defun ledgify-buffer ()
  "Convert buffer items to ledger entries"
  (interactive)
  (with-output-to-temp-buffer "*ledger-output*"
      (mapc #'princ
            (mapcar #'mjr/ledger-item
                    (mapcar '(lambda (line)
                               (mjr/statement-item smile-regex line))
                            (split-string (buffer-string) "\n" t))))))

(provide 'mjr-ledger)

