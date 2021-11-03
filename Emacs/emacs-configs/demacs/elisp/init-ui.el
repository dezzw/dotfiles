;;; init-ui.el --- UI -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Desmond Wang
;;
;; Author: Desmond Wang <https://github.com/dez>
;; Maintainer: Desmond Wang <desmoond.pc.w@gmail.com>
;; Created: October 10, 2021
;; Modified: October 10, 2021
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/dez/init-ui
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  UI
;;
;;; Code:

(column-number-mode)

;; Enable liner number
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                vterm-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(defun dw/set-font-faces ()
  (message "Setting faces!")
  ;; set font
  ;; (set-face-attribute 'default nil :font "JetBrains Mono" :height 150)
  (set-face-attribute 'default nil :font "Fira Code" :height 150)
  
  ;; Set the fixed pitch face
  ;; (set-face-attribute 'fixed-pitch nil :font "JetBrains Mono" :height 150)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code" :height 150)

  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font "Cantarell" :height 150 :weight 'regular))

(push '(use-package ligature
	 :straight (ligature.el :type git :host github :repo "mickeynp/ligature.el")
	 :config
	 ;; Enable the "www" ligature in every possible major mode
	 (ligature-set-ligatures 't '("www"))
	 ;; Enable traditional ligature support in eww-mode, if the
	 ;; `variable-pitch' face supports it
	 (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
	 ;; Enable all Cascadia Code ligatures in programming modes
	 (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
					      ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
					      "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
					      "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
					      "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
					      "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
					      "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
					      "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
					      ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
					      "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
					      "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
					      "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
					      "\\\\" "://"))
	 ;; Enables ligature checks globally in all buffers. You can also do it
	 ;; per mode with `ligature-mode'.
	 (global-ligature-mode t)) gui-only-plugins-setting)


(push '(use-package all-the-icons
         :custom
         (all-the-icons-dired-monochrome t)) gui-only-plugins-setting)

(use-package doom-themes
  :config
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
    ;; (doom-themes-org-config)
  )

;; (if (not (display-graphic-p))
;;     (load-theme 'doom-one t))

;; (if (or (display-graphic-p) (not (string= (daemonp) "tty")))
;;     (defun dw/apply-theme (appearance)
;;       "Load theme, taking current system APPEARANCE into consideration."
;;       (mapc #'disable-theme custom-enabled-themes)
;;       (pcase appearance
;; 	('light (load-theme 'doom-solarized-light t))
;; 	('dark (load-theme 'doom-one t)))))

(load-theme 'doom-one t)

(use-package page-break-lines)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-window-width-limit fill-column)
  (doom-modeline-icon (display-graphic-p)))

(use-package dashboard
  :custom
  ;; Set the banner
  (dashboard-startup-banner "~/.dotfiles/Emacs/dashboard/banner.txt")
  (dashboard-center-content t)
  (dashboard-items '((recents  . 7)
                     (projects . 5)
                     (agenda . 3)
                     ))
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-week-agenda nil)
  (dashboard-agenda-release-buffers t)
  :config
  (dashboard-setup-startup-hook))

(push '(use-package nyan-mode
	 :defer t
	 :custom
	 (nyan-mode t)
	 (nyan-animate-nyancat t)
	 (nyan-wavy-trail t)) gui-only-plugins-setting)

(use-package hl-todo
  :hook ((org-mode prog-mode  lsp-mode) . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
        '(("TODO"   . "#FF0000")
          ("FIXME"  . "#FF0000")
          ("DEBUG"  . "#A020F0")
          ("NEXT" . "#FF4500")
	  ("TBA" . "#61d290")
          ("UNCHECK"   . "#1E90FF"))))

(use-package highlight-numbers
  :hook ((prog-mode  lsp-mode) . highlight-numbers-mode))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
		(add-hook 'ns-system-appearance-change-functions #'dw/apply-theme)
		(dashboard-setup-startup-hook)
                (with-selected-frame frame
                  (dw/set-font-faces))
		(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
		))
  (add-hook 'ns-system-appearance-change-functions #'dw/apply-theme)
  (if (display-graphic-p)
      (dw/set-font-faces)))

(use-package smooth-scrolling
  :defer t
  :config
  (smooth-scrolling-mode 1))


(provide 'init-ui)
;;; init-ui.el ends here
