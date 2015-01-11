;; mail related functions

;; message mode setup
(add-hook 'message-mode-hook
          '(lambda () (progn
                        (turn-on-flyspell)
                        (turn-on-orgstruct++))))

;; message signature
(defun mjr/sig ()
  (let ((sigs
        '("You used to be alright. What happened?"
          "Avoid success at all costs"
          "Who's number one with the big cigar?"
          "Jive ass dude don't got no brains anyhow!"
          "A woman is the most fiendish instrument of torture ever devised to bedevil the days of man"
          "I'm not sure I agree with you a hundred percent on your police work, there, Lou"
          "Confucious he say: 'Name go in book'"))
        (me "Matthew Ryall\nNet-A-Porter\n020 7198 4669\n\n"))
    (format "%s" (nth (random (length sigs)) sigs))))

;;(setq message-signature 'mjr/sig)
(setq message-signature nil)

(setq message-confirm-send t)

;;

(provide 'mjr-mail)
