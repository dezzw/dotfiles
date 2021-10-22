;;; init-note.el --- Note -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-note
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Note
;;
;;; Code:

(use-package deft
  :commands (deft)
  :config (setq deft-directory "~/Documents/Org/Notes"
                deft-recursive t
                deft-extensions '("md" "org"))

  ;;https://github.com/jrblevin/deft/issues/75#issuecomment-905031872
  (defun cm/deft-parse-title (file contents)
    "Parse the given FILE and CONTENTS and determine the title.
  If `deft-use-filename-as-title' is nil, the title is taken to
  be the first non-empty line of the FILE.  Else the base name of the FILE is
  used as title."
    (let ((begin (string-match "^#\\+[tT][iI][tT][lL][eE]: .*$" contents)))
      (if begin
	  (string-trim (substring contents begin (match-end 0)) "#\\+[tT][iI][tT][lL][eE]: *" "[\n\t ]+")
	(deft-base-filename file))))

  (advice-add 'deft-parse-title :override #'cm/deft-parse-title)

  (setq deft-strip-summary-regexp
	(concat "\\("
		"[\n\t]" ;; blank
		"\\|^#\\+[[:alpha:]_]+:.*$" ;; org-mode metadata
		"\\|^:PROPERTIES:\n\\(.+\n\\)+:END:\n"
		"\\)"))
  )


(provide 'init-note)
;;; init-note.el ends here
