
;; Perl

;; use local::lib
(setenv "MODULEBUILDRC" "/home/mryall/perl5/.modulebuildrc")
(setenv "PERL_MM_OPT" "INSTALL_BASE=/home/mryall/perl5")
(setenv "PERL5LIB"
        "/home/mryall/perl5/lib/perl5:/home/mryall/perl5/lib/perl5/x86_64-linux-gnu-thread-multi")

(setenv "PATH" (concat "/opt/xt/xt-perl/bin/perl:/home/mryall/perl5/bin:" (getenv "PATH")))

;; use cperl mode
(defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face 'default)
(setq cperl-indent-level 4)
(setq cperl-indent-parens-as-block t)
(require 'smart-compile+)

;; cperl for tests too
(add-to-list 'auto-mode-alist '("\\.t\\'" . cperl-mode))

;; perldoc
(add-hook 'cperl-mode-hook
          (lambda ()
            (local-set-key (kbd "C-h f") 'cperl-perldoc)))

;; perltidy
(defun perltidy-region ()
   "Run perltidy on the current region."
   (interactive)
   (save-excursion
     (shell-command-on-region (point) (mark) "perltidy -q" nil t)))

(defun perltidy-buffer ()
   "Run perltidy on the current buffer."
   (interactive)
   (save-excursion
     (shell-command-on-region (point-min) (point-max) "perltidy -q" nil t)))

(defun perltidy-defun ()
  "Run perltidy on the current defun."
  (interactive)
  (save-excursion (mark-defun)
                  (perltidy-region)))

(eval-after-load "cperl-mode"
  '(add-hook 'cperl-mode-hook
             (lambda () (local-set-key "\C-ct" 'cperl-prove))))

(defun cperl-prove ()
  "Run the current test."
  (interactive)
  (shell-command (concat "prove -lv --merge -I t/ "
                         (shell-quote-argument (buffer-file-name)))))

(add-hook 'cperl-mode-hook
          '(lambda ()
             (define-key cperl-mode-map "\C-c\C-c"
               'stylish-repl-send-region-to-stylish)
             (define-key cperl-mode-map "\C-c r" 'smart-run)))

;;

(provide 'mjr-perl)
