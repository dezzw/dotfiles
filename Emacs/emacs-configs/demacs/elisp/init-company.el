;;; init-company.el --- Company Configration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-company
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Company Configration
;;
;;; Code:

(use-package company
  :hook ((lsp-mode prog-mode conf-mode) . company-mode)
  :custom
  (company-tooltip-align-annotations t)
  ;; ;; Number the candidates (use M-1, M-2 etc to select completions)
  (company-show-numbers t)
  ;; ;; starts with 1 character
  (company-minimum-prefix-length 1)
  ;; ;; Trigger completion immediately
  (company-idle-delay 0.2)
  ;; ;; Back to top when reach the end
  (company-selection-wrap-around t)
  :config
  ;; Use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  ;; (company-tng-configure-default)
  ;; (require 'company_init)
  )

;;Completion based on AI
(use-package company-tabnine
  :after lsp
  :config
  (push '(company-capf :with company-tabnine :separate company-yasnippet :separete) company-backends))

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package citre
  :commands (citre-jump citre-ace-peek)
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.
  :bind (("C-x c j" . 'citre-jump)
	 ("C-x c J" . 'citre-jump-back)
	 ("C-x c p" .  'citre-ace-peek))
  :custom
  ;; Set this if you use project management plugin like projectile.  It's
  ;; only used to display paths relatively, and doesn't affect actual use.
  (citre-project-root-function #'dw/get-project-root)
  (citre-use-project-root-when-creating-tags t)
  (citre-prompt-language-for-ctags-command t)
  (citre-auto-enable-citre-mode-modes '(prog-mode)))

(use-package yasnippet
  :defer t
  :hook ((org-mode lsp-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
        '("~/.dotfiles/Emacs/snippets"))
  (yas-reload-all))

;; Snippets Collection
(use-package yasnippet-snippets
  :after yasnippet)

;; auto insert
(use-package auto-yasnippet
  :disabled
  :after yasnippet)

(provide 'init-company)
;;; init-company.el ends here
