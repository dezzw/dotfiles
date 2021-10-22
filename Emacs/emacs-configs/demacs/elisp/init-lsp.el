;;; init-lsp.el --- LSP -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-lsp
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  LSP
;;
;;; Code:

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (((sh-mode typescript-mode js2-mode web-mode css-mode Latex-mode TeX-latex-mode c-mode cc-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-log-io nil)
  (lsp-idle-delay 0.500)
  (lsp-completion-provider :capf)
  :config
  (add-to-list 'lsp-language-id-configuration '(scss-mode . "css"))
  (add-to-list 'lsp-language-id-configuration '(less-css-mode . "css")))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-imenu-auto-refresh t)
  )

(use-package lsp-ivy
  :disabled
  :after lsp
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :commands dap-debug
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip))
  :config
  ;; Set up python debugging
  (require 'dap-python)

  ;; Set up chrome debugging
  (require 'dap-chrome)
  (dap-chrome-setup)

  ;; Set up node debugging
  (require 'dap-node)
  (dap-node-setup)

  (require 'dap-java))

(provide 'init-lsp)
;;; init-lsp.el ends here
