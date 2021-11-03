;;; init-tex.el --- LaTex -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-tex
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  LaTex
;;
;;; Code:

(straight-use-package 'auctex)

(use-package latex-preview-pane
    :commands (latex-preview-pane-mode latex-preview-pane-update))

(use-package cdlatex
  :hook
  (org-mode . org-cdlatex-mode)
  (LaTeX-mode . cdlatex-mode)
  (latex-mode . cdlatex-mode))

;; (use-package xenops
;;   :hook ((latex-mode LaTeX-mode) . xenops-mode)
;;   )

(provide 'init-tex)
;;; init-tex.el ends here
