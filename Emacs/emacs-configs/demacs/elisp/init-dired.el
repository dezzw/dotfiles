;;; init-dired.el --- Dired -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-dired
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Dired
;;
;;; Code:

(use-package dired
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  )

(use-package dired-single
  :commands (dired dired-jump))

(push '(use-package all-the-icons-dired
         :hook (dired-mode . all-the-icons-dired-mode)) gui-only-plugins-setting)

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  )

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  )

(provide 'init-dired)
;;; init-dired.el ends here
