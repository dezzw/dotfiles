;;; init-js.el --- JS -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-js
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  JS
;;
;;; Code:

(use-package nvm
  :after (typescript-mode js2-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.m?js\\'"
  :config

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  )

(use-package rjsx-mode
  :mode "\\.jsx\\'")

(use-package prettier-js
  :disabled
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; This gives you a tab of 2 spaces
  (custom-set-variables '(coffee-tab-width 2))

  (use-package sourcemap)
  ;; generating sourcemap by '-m' option. And you must set '--no-header' option
  (setq coffee-args-compile '("-c" "--no-header" "-m"))
  (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

  ;; If you want to remove sourcemap file after jumping corresponding point
  (defun my/coffee-after-compile-hook (props)
    (sourcemap-goto-corresponding-point props)
    (delete-file (plist-get props :sourcemap)))
  (add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)
  )

(use-package flymake-coffee
  :hook (coffee-mode . flymake-coffee))


(provide 'init-js)
;;; init-js.el ends here
