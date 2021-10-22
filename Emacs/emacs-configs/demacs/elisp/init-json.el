;;; init-json.el --- JSON -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-json
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  JSON
;;
;;; Code:

(use-package json-mode
  :mode "\\.json\\'"
  :config
	;; Set up proper indentation in JSON
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(provide 'init-json)
;;; init-json.el ends here
