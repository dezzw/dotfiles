(setq gui-only-plugins-setting ())
(setq tui-only-plugins-setting ())

(require 'init-ui)

;; Editing
(require 'init-meow)
(require 'init-edit)

;; Windows
(require 'init-win)

;; Dired
(require 'init-dired)

;; Project
(require 'init-project)

;; Completion
(require 'init-completion)

;; Org
(require 'init-org)
(require 'init-note)

;; Markdown
(require 'init-md)

;; Language
(require 'init-company)
(require 'init-lsp)
(require 'init-py)
(require 'init-c)
(require 'init-java)
(require 'init-js)
(require 'init-json)
(require 'init-web)
(require 'init-r)
(require 'init-yaml)
(require 'init-tex)

;; Tool
(require 'init-tool)
(require 'init-git)
(require 'init-pass)
(require 'init-term)
(require 'init-tmux)
(require 'init-leetcode)

(if (or (display-graphic-p) (and (daemonp) (not (string= (daemonp) "tty"))))
    (dolist (gui-plugins gui-only-plugins-setting)
      (eval gui-plugins)))

;; TUI Only Plugins
(if (or (not (display-graphic-p)) (string= (daemonp) "tty"))
    (dolist (tui-plugins tui-only-plugins-setting)
      (eval tui-plugins)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-show-quick-access t nil nil "Customized with use-package company")
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
