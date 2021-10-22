;;; init-web.el --- Web-HTML-CSS -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-web
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Web-HTML-CSS
;;
;;; Code:

(use-package web-mode
    :mode "\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'")

  ;; Preview the html file
  (use-package skewer-mode
    :after web-mode
    :config
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode)
    (add-hook 'web-mode-hook 'skewer-html-mode))

(use-package emmet-mode
      :hook (web-mode . emmet-mode))

(use-package scss-mode
  :mode "\\.scss\\'"
  :custom
  (scss-compile-at-save t)
  (scss-output-directory "../css")
  (scss-sass-command "sass --no-source-map"))

(provide 'init-web)
;;; init-web.el ends here
