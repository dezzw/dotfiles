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
(require 'init-env)
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
(require 'init-nix)

;; Tool
(require 'init-tool)
(require 'init-git)
(require 'init-pass)
(require 'init-term)
(require 'init-eshell)
(require 'init-tmux)
(require 'init-leetcode)

(require 'init-eaf)

(if (or (display-graphic-p) (and (daemonp) (not (string= (daemonp) "tty"))))
    (dolist (gui-plugins gui-only-plugins-setting)
      (eval gui-plugins)))

;; TUI Only Plugins
(if (or (not (display-graphic-p)) (string= (daemonp) "tty"))
    (dolist (tui-plugins tui-only-plugins-setting)
      (eval tui-plugins)))
