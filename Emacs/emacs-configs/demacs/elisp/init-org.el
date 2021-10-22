;;; init-org.el --- Org Mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-org
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Org Mode
;;
;;; Code:

;; Since we don't want to disable org-confirm-babel-evaluate all
;; of the time, do it around the after-save-hook
(defun dw/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dw/org-babel-tangle-dont-ask
                                         'run-at-end 'only-in-org-mode)))

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-html-head-include-default-style nil)
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  (setq org-html-htmlize-output-type nil)

  ;; config for images in org
  (auto-image-file-mode t)
  (setq org-image-actual-width nil)
  ;; default image width
  (setq org-image-actual-width '(300))

  (setq org-export-with-sub-superscripts nil)

  ;; 不要自动创建备份文件
  (setq make-backup-files nil)

  (with-eval-after-load "meow"
    (meow-leader-define-key
     '("a" . org-agenda))))

;; change bullets for headings
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dw/org-mode-visual-fill))

(push '(use-package valign
         :hook (org-mode . valign-mode)
         ) gui-only-plugins-setting)

(with-eval-after-load "org-export-dispatch"
  ;; Edited from http://emacs.stackexchange.com/a/9838
  (defun dw/org-html-wrap-blocks-in-code (src backend info)
    "Wrap a source block in <pre><code class=\"lang\">.</code></pre>"
    (when (org-export-derived-backend-p backend 'html)
      (replace-regexp-in-string
       "\\(</pre>\\)" "</code>\n\\1"
       (replace-regexp-in-string "<pre class=\"src src-\\([^\"]*?\\)\">"
				 "<pre>\n<code class=\"\\1\">" src))))

  (require 'ox-html)

  (add-to-list 'org-export-filter-src-block-functions
               'dw/org-html-wrap-blocks-in-code))

(with-eval-after-load "org"
  (use-package ob-browser
    :defer t)

  (with-eval-after-load "ob"
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (latex . t)
       (java . t)
       (C . t)
       (js . t)
       (browser . t)
       (python . t)
       (R .t)))
    )

  (setq org-confirm-babel-evaluate nil)
  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("java" . "src java"))
  (add-to-list 'org-structure-template-alist '("srcc" . "src C"))
  (add-to-list 'org-structure-template-alist '("cpp" . "src cpp"))
  (add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
  (add-to-list 'org-structure-template-alist '("js" . "src js"))
  (add-to-list 'org-structure-template-alist '("css" . "src css"))
  (add-to-list 'org-structure-template-alist '("html" . "src browser :out"))
  (add-to-list 'org-structure-template-alist '("py" . "src python :results output :exports both"))
  (add-to-list 'org-structure-template-alist '("la" . "latex"))
  (add-to-list 'org-structure-template-alist '("r" . "src R"))
  ;; (add-to-list 'org-structure-template-alist '("d" . "src ditaa :file ../images/.png :cmdline -E"))
  )

(use-package org-download
  :disabled
  ;;将截屏功能绑定到快捷键：Ctrl + Shift + Y
  :bind ("C-S-y" . org-download-screenshot)
  :config
  (require 'org-download)
  ;; Drag and drop to Dired
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Documents/Org/Notes")
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point)
	 )
  :config
  (org-roam-db-autosync-mode)
  ;; (org-roam-setup)
  (require 'org-roam-protocol))

(with-eval-after-load 'org-agenda
  (require 'init-org-agenda))

(provide 'init-org)
;;; init-org.el ends here
