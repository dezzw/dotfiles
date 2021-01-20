;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)

(menu-bar-mode -1)            ; Disable the menu bar

(setq ring-bell-function 'ignore)

;; Find Executable Path on OS X
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  )

;; 关闭备份
(setq make-backup-files nil
      auto-save-default nil)

(setq auto-window-vscroll nil)

;; set delete selection mode
(delete-selection-mode t)

;; Since we don't want to disable org-confirm-babel-evaluate all
;; of the time, do it around the after-save-hook
(defun dw/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dw/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode)))

(global-auto-revert-mode 1)

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t))

(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; Set frame transparency and maximize windows by default.

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Enalbe column number
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

;; set font
(set-face-attribute 'default nil :font "Jetbrains Mono" :height 140)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Jetbrains Mono" :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 175 :weight 'regular)

(use-package all-the-icons)

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-env-version t))

(use-package dashboard
  :init
  ;; Set the title
  (setq dashboard-banner-logo-title nil)
  ;; Set the banner
  (setq dashboard-startup-banner "~/.emacs.d/dashboard/banner.txt")
  (setq dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 5)
                          (projects . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-evil-setup t)

  (general-create-definer dw/leader-key-def
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer dw/ctrl-c-keys
    :prefix "C-c"))

(defun dw/evil-hook ()
  (dolist (mode '(custom-mode
                  eshell-mode
		  vterm-mode
                  term-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :config
  (add-hook 'evil-mode-hook 'dw/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p nil)
  :config
  (evil-collection-init))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :defer t
  :hook (org-mode . dw/org-mode-setup)
  :config
  (setq org-html-head-include-default-style nil)
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (latex . t)
   (python . t)))

 (setq org-confirm-babel-evaluate nil)
 (push '("conf-unix" . conf-unix) org-src-lang-modes)

;; change bullets for headings
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen(-) with dot(.)
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                          (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Make sure org faces is available
(require 'org-faces)
;; Make sure org-indent face is available
(require 'org-indent)
;; Set Size and Fonts for Headings
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.1)
                (org-level-3 . 1.05)
                (org-level-4 . 1.0)
                (org-level-5 . 1.1)
                (org-level-6 . 1.1)
                (org-level-7 . 1.1)
                (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; This is needed as of Org 9.2
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("js" . "src javascript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("la" . "latex"))

(defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . dw/org-mode-visual-fill))

(setq org-image-actual-width nil)

(use-package org-download
	  :ensure t 
	  ;;将截屏功能绑定到快捷键：Ctrl + Shift + Y
	  :bind ("C-S-y" . org-download-screenshot)
	  :config
	  (require 'org-download)
	  ;; Drag and drop to Dired
	  (add-hook 'dired-mode-hook 'org-download-enable))

(auto-image-file-mode t)

(use-package htmlize)

(use-package cdlatex
  :hook (org-mode . org-cdlatex-mode)
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-cdlatex))

(use-package org-latex-impatient
  :defer t
  :hook (org-mode . org-latex-impatient-mode)
  :init
  (setq org-latex-impatient-tex2svg-bin
        ;; location of tex2svg executable
        "~/.nvm/versions/node/v15.5.1/lib/node_modules/mathjax-node-cli/bin/tex2svg")
  :custom
  (org-latex-impatient-posframe-position-handler 'posframe-poshandler-point-bottom-left-corner))

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Documents/Wiki")
      :bind (:map org-roam-mode-map
              (("C-c n l" . org-roam)
               ("C-c n f" . org-roam-find-file)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate))))

(use-package org-roam-server
  :ensure t
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-always t))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single)

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package edit-indirect)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-switch-buffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-histor))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^


(dw/leader-key-def
  "SPC" 'counsel-M-x)

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-posframe
 :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~Documents/Projects/Code")
    (setq projectile-project-search-path '("~Documents/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :after evil
  :config
  (evil-escape-mode t)
  (setq-default evil-escape-key-sequence "jk"))

(use-package evil-nerd-commenter
  :after evil
  :config
  (global-set-key (kbd "M-;") 'evilnc-comment-or-uncomment-lines)
  (global-set-key (kbd "C-c l") 'evilnc-quick-comment-or-uncomment-to-the-line)
  (global-set-key (kbd "C-c c") 'evilnc-copy-and-comment-lines)
  (global-set-key (kbd "C-c p") 'evilnc-comment-or-uncomment-paragraphs))

(use-package vterm
  :defer t)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((sh-mode typescript-mode js-mode web-mode python-mode css-mode Latex-mode TeX-latex-mode) . lsp)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-log-io t)
  (setq lsp-idle-delay 0.500)
  (setq lsp-completion-provider :capf))

(dw/leader-key-def
  "l"  '(:ignore t :which-key "lsp")
  "ld" 'xref-find-definitions
  "lr" 'xref-find-references
  "ln" 'lsp-ui-find-next-reference
  "lp" 'lsp-ui-find-prev-reference
  "ls" 'counsel-imenu
  "le" 'lsp-ui-flycheck-list
  "lS" 'lsp-ui-sideline-mode
  "lX" 'lsp-execute-code-action)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-sideline-enable t)
  (setq lsp-ui-doc-position 'bottom))

(use-package lsp-ivy 
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 4))

(defun dw/set-js-indentation ()
  (setq js-indent-level 4)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 4))

(use-package js2-mode)
  ;; :mode "\\.jsx?\\'")

;; Don't use built-in syntax checking
(setq js2-mode-show-strict-warnings nil)

;; Set up proper indentation in JavaScript and JSON files
(add-hook 'js2-mode-hook #'dw/set-js-indentation)
(add-hook 'json-mode-hook #'dw/set-js-indentation)

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package web-mode
  :mode "\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'")

;; Impatient Html File
(use-package impatient-mode)

;; Preview the html file
(use-package skewer-mode
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode)
  (add-hook 'web-mode-hook 'skewer-html-mode))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package lsp-latex
  :config
  (add-hook 'tex-mode-hook 'lsp)
  (add-hook 'latex-mode-hook 'lsp)
  (add-hook 'Latex-mode-hook 'lsp)
  (add-hook 'TeX-latex-mode-hook 'lsp))

(use-package lsp-sourcekit
  :after lsp-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :hook (swift-mode . (lambda () (lsp))))

(use-package company 
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind 
  (:map company-active-map
        ("<tab>". company-complete-selection))
  (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
  :config
  ;; (setq global-company-mode t)
  :custom
  ;; Number the candidates (use M-1, M-2 etc to select completions)
  (company-show-numbers t)
  ;; starts with 1 character
  (company-minimum-prefix-length 1)
  ;; Trigger completion immediately
  (company-idle-delay 0))

;; Add UI for Company
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

;;Completion based on AI
(use-package company-tabnine
  :after lsp-mode)

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

;; ask flycheck uses pylint in the current virtualenv
;; (declare-function python-shell-calculate-exec-path "python")

;; (defun flycheck-virtualenv-executable-find (executable)
;;   "Find an EXECUTABLE in the current virtualenv if any."
;;   (if (bound-and-true-p python-shell-virtualenv-root)
;;       (let ((exec-path (python-shell-calculate-exec-path)))
;;         (executable-find executable))
;;     (executable-find executable)))

;; (defun flycheck-virtualenv-setup ()
;;   "Setup Flycheck for the current virtualenv."
;;   (setq-local flycheck-executable-find #'flycheck-virtualenv-executable-find))

(use-package yasnippet
  :after prog-mode
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

;; Snippets Collection
(use-package yasnippet-snippets)

;; auto insert
(use-package auto-yasnippet)

(dw/leader-key-def
  "a"  '(:ignore t :which-key "auto-snippets")
  "aw" 'aya-create
  "ay" 'aya-expand
  "ao" 'aya-open-line)

;; (push '(company-capf :with company-tabnine :separate) company-backends)
;; company-dabbrev 
 ;; company-yasnippet :separate
(push '(company-dabbrev :with company-capf company-yasnippet :separate) company-backends)

;; dap debug tools
(use-package dap-mode
  :after lsp-mode
  :config
  (require 'dap-python)
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))


(dw/leader-key-def
  "d"  '(:ignore t :which-key "dap debug")
  "dd" 'dap-debug
  "da" 'dap-breakpoint-add
  "dsc" 'dap-breakpoint-delete
  "dsc" 'dap-breakpoinnt-delete-all
  "di" 'dap-step-in
  "do" 'dap-step-out
  "dn" 'dap-next)

(use-package smartparens
  :after prog-mode
  :hook (prog-mode . smartparens-mode))

(use-package rainbow-delimiters
  :after prog-mode
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hungry-delete
  :after prog-mode
  :hook ('prog-mode . 'global-hungry-delete-mode))

(use-package indent-guide
  :after prog-mode
  :hook (prog-mode . indent-guide-mode))

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

(use-package format-all
  :after prog-mode)

(use-package magit
  :defer t
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package evil-magit
  :after magit)

;; Add a super-convenient global binding for magit-status since
;; I use it 8 million times a day
(global-set-key (kbd "C-M-;") 'magit-status)

;; Enable to control pipenv in Emacs
(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode)) 


(dw/leader-key-def
  "p"  '(:ignore t :which-key "pyenv")
  "pp" 'pyenv-mode
  "ps" 'pyenv-mode-set
  "pu" 'pyenv-mode-unset
  "pr" 'run-python)

;; auto activates the virtual environment if .python-version exists
;;(use-package pyenv-mode-auto)

;; (use-package shim
;;   :load-path "~/.emacs.d/site-packages/shim/shim.el"
;;   :hook (python-mode . shim-mode)
;;   :config
;;   (shim-init-python))

(use-package auto-virtualenvwrapper
  :after python-mode
  :config
  (add-hook 'python-mode-hook #'auto-virtualenvwrapper-activate)
  (add-hook 'window-configuration-change-hook #'auto-virtualenvwrapper-activate)
  (add-hook 'focus-in-hook #'auto-virtualenvwrapper-activate))

(use-package quickrun
  :after prog-mode
  :config
  ;; set python3 as default
  (quickrun-add-command "python" 
    '((:command . "python3") 
      (:exec . "%c %s") 
      (:tempfile . nil)) 
    :default "python"))

;; Set up Keybindings
  (dw/leader-key-def
  "r"  '(:ignore t :which-key "quickrun")
  "rr" 'quickrun
  "ra" 'quickrun-with-arg
  "rs" 'quickrun-shell
  "rc" 'quickrun-compile-only
  "re" 'quickrun-region)
