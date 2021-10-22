;;; init-project.el --- Project Management -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-project
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Project Management
;;
;;; Code:

(use-package projectile
   :diminish projectile-mode
   :defer 1
   :config (projectile-mode)
   :bind-keymap
   ("C-c p" . projectile-command-map)
   :init
   (when (file-directory-p "~/Documents/Projects/Code")
     (setq projectile-project-search-path '("~/Documents/Projects/Code")))
   (setq projectile-switch-project-action #'projectile-dired))



(provide 'init-project)
;;; init-project.el ends here
