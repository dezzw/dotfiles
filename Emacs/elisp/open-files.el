(defun open-init()
  (interactive)
  (find-file "~/.dotfiles/Emacs/demacs.org")
  )

(defun open-utm()
  (interactive)
  (dired "/Users/desmond/Library/Mobile Documents/com~apple~CloudDocs/UTM")
  )

(defun open-org()
  (interactive)
  (dired "/Users/desmond/Documents/Org")
  )

(meow-leader-define-key
 '("oi" . open-init)
 '("ou" . open-utm)
 '("oo" . open-org)
 )

(provide 'open-files)

