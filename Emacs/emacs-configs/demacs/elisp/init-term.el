;;; init-term.el --- Terminal -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-term
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Terminal
;;
;;; Code:

(when (and module-file-suffix           ; dynamic module
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :defer 1
    :init
    (setq vterm-always-compile-module t)

    (with-no-warnings
      (when (posframe-workable-p)
        (defvar vterm-posframe--frame nil)
        (defun vterm-posframe-toggle ()
          "Toggle `vterm' child frame."
          (interactive)
          (let ((buffer (vterm--internal #'ignore 100))
                (width  (max 80 (/ (frame-width) 2)))
                (height (/ (frame-height) 2)))
            (if (frame-live-p vterm-posframe--frame)
                (progn
                  (posframe-delete-frame buffer)
                  (setq vterm-posframe--frame nil))
              (setq vterm-posframe--frame
                    (posframe-show
                     buffer
                     :poshandler #'posframe-poshandler-frame-center
                     :left-fringe 8
                     :right-fringe 8
                     :width width
                     :height height
                     :min-width width
                     :min-height height
                     :internal-border-width 3
                     :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                     :background-color (face-background 'tooltip nil t)
                     :accept-focus t)))))
        (bind-key "C-`" #'vterm-posframe-toggle)))))


(use-package multi-vterm
  :commands multi-vterm)

(use-package vterm-toggle
  :commands vterm-toggle)

(meow-leader-define-key
 '("tt" . vterm-toggle)
 '("tm" . multi-vterm)
 '("tp" . vterm-posframe-toggle)
 )

(provide 'init-term)
;;; init-term.el ends here
