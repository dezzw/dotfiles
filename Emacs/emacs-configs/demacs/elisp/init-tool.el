;;; init-tool.el --- Utilits Tools -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-tool
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Utilits Tools
;;
;;; Code:

(show-paren-mode t)

(use-package smartparens
  :hook ((prog-mode lsp-mode) . smartparens-mode)
  :init
  (require 'smartparens-config)
  :config
  (define-key smartparens-mode-map (kbd "M-r") #'sp-rewrap-sexp)
  (define-key smartparens-mode-map (kbd "M-s") #'sp-unwrap-sexp)
  (define-key smartparens-mode-map (kbd "M-[") #'sp-wrap-square)
  (define-key smartparens-mode-map (kbd "M-{") #'sp-wrap-curly)
  (define-key smartparens-mode-map (kbd "C-)") #'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "C-}") #'sp-forward-barf-sexp))

(use-package rainbow-delimiters
  :hook ((prog-mode lsp-mode) . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook ((org-mode prog-mode lsp-mode) . rainbow-mode))

(use-package hungry-delete
  :hook ((prog-mode lsp-mode) . hungry-delete-mode))

(use-package indent-guide
  :disabled
  :hook (lsp-mode . indent-guide-mode))

(use-package highlight-indent-guides
  :hook ((prog-mode lsp-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-method 'character))

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode))

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode))

(use-package format-all
    :hook ((prog-mode lsp-mode) . format-all-mode)
    :commands (format-all-ensure-formatter format-all-buffer))

(use-package quickrun
    :commands (quickrun)
    :config
    ;; set python3 as default
    (quickrun-add-command "python"
      '((:command . "python3")
        (:exec . "%c %s")
        (:tempfile . nil))
      :default "python"))

(use-package flycheck
      :hook (lsp-mode . flycheck-mode))

(use-package minimap
  :commands (minimap-mode)
  :custom
  (minimap-window-location 'right))

(use-package treemacs
  :commands (treemacs))

(use-package treemacs-all-the-icons
  :disabled
  :after treemacs)

(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

(use-package cliphist
	:commands (cliphist-paste-item cliphist-select-item))

(use-package diff-hl
  :hook (dired-mode . diff-hl-dired-mode-unless-remote)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :hook (prog-mode . diff-hl-mode)
  :config
  ;; use margin instead of fringe
  (diff-hl-margin-mode))

(use-package osx-trash
  :defer 1
  :config
  (when (eq system-type 'darwin)
  (osx-trash-setup))
  (setq delete-by-moving-to-trash t))

(use-package fzf
  :commands (fzf))

(push '(use-package clipetty
      :hook (after-init . global-clipetty-mode)
      ) tui-only-plugins-setting)

(if (daemonp)
    (use-package emacs-everywhere))


(provide 'init-tool)
;;; init-tool.el ends here
