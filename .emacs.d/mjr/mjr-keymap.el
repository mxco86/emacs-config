
;; control keys
(global-set-key "\C-x\C-m" 'smex)
(global-set-key "\C-c\C-m" 'smex)
(global-set-key "\C-xq"    'help-command)
(global-set-key "\C-w"     'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-xr"    'query-replace-regexp)
(global-set-key "\C-xl"    'goto-line )
(global-set-key "\C-cb"    'bury-buffer )
(global-set-key "\C-cp"    'ps-print-buffer-with-faces )
(global-set-key "\C-cf"    'revert-buffer )
(global-set-key "\C-xj"    'jump-to-register )
(global-set-key "\C-csw"   'flyspell-auto-correct-word )
(global-set-key "\r"       'newline-and-indent)
(global-set-key "\M-/"     'hippie-expand)

(provide 'mjr-keymap)
