;;; init-leetcode.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-leetcode
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package leetcode
  :commands (leetcode start-leetcode)
  :custom
  (leetcode-prefer-language "python3")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  (leetcode-directory "~/Documents/leetcode"))

(defun start-leetcode()
    (interactive)
    (global-display-line-numbers-mode -1)
    (display-line-numbers-mode -1)
    (leetcode))

(defun quit-leetcode()
  (interactive)
  (leetcode-quit)
  (global-line-numebrs-mode t))

(provide 'init-leetcode)
;;; init-leetcode.el ends here
