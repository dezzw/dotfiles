;;; init-meow.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-meow
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'init-meow-qwerty)

(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-setup)
  ;; 如果你需要在 NORMAL 下使用相对行号（基于 display-line-numbers-mode）
  (meow-setup-line-number)
  ;;:bind ("" . meow-insert-exit)
  (add-to-list 'meow-mode-state-list '(inferior-emacs-lisp-mode . normal))
  (add-to-list 'meow-mode-state-list '(org-agenda-mode . normal))
  )

(meow-leader-define-key
 '("f" . find-file)
 '("b" . switch-to-buffer)
 '("qr" . quickrun)
 '("wo" . ace-window)
 '("wd" . ace-delete-window)
 '("wt" . treemacs-select-window)
 '("dd" . dap-debug)
)

(require 'open-files)

(meow-motion-overwrite-define-key
 '("h" . dired-single-up-directory)
 '("l" . dired-single-buffer))


(provide 'init-meow)
;;; init-meow.el ends here
