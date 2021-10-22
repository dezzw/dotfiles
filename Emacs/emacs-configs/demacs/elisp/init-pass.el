;;; init-pass.el --- Pass -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-pass
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Pass
;;
;;; Code:

(use-package password-store
  :commands (password-store-copy password-store-insert)
  :config
  (setq password-store-password-length 12))

(use-package auth-source-pass
  :disabled
  :config
  (auth-source-pass-enable))

(provide 'init-pass)
;;; init-pass.el ends here
