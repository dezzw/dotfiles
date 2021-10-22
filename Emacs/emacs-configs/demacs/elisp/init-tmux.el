;;; init-tmux.el --- Tmux -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-tmux
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Tmux
;;
;;; Code:

(push '(use-package emamux
	   :bind ("C-z" . emamux:keymap)
	   ;; :config
	   ;; (global-set-key (kbd "C-z") emamux:keymap)
	   ) tui-only-plugins-setting)

(push '(use-package tmux-pane
      :disabled
      :config
      (tmux-pane-mode)
      ) tui-only-plugins-setting)

(provide 'init-tmux)
;;; init-tmux.el ends here
