;;; init-edit.el --- Better Editing -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-edit
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Better Editing
;;
;;; Code:

;; set delete selection mode
(delete-selection-mode t)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package multiple-cursors
  :commands (mc/edit-lines mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :bind
  (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-S-c C-<" . 'mc/mark-all-like-this)))

(use-package iedit
  :after lsp)

(use-package evil-nerd-commenter
  :commands (evilnc-comment-or-uncomment-lines)
  :bind
  ("M-;" . 'evilnc-comment-or-uncomment-lines)
  ("C-c l" . 'evilnc-quick-comment-or-uncomment-to-the-line)
  ("C-c c" . 'evilnc-copy-and-comment-lines)
  ("C-c p" . 'evilnc-comment-or-uncomment-paragraphs)
  )

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line))

(meow-leader-define-key
 '("tc" . avy-goto-char)
 '("tw" . avy-goto-word-0)
 '("tl" . avy-goto-line)
 )

(use-package avy
  :commands (avy-goto-char avy-goto-word-0 avy-goto-line)
  :config
  (meow-leader-define-key
   '("tc" . avy-goto-char)
   '("tw" . avy-goto-word-0)
   '("tl" . avy-goto-line)
   ))







(provide 'init-edit)
;;; init-edit.el ends here
