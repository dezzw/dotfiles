(setq graphic-only-plugins-setting ())

(setq comp-async-jobs-number 7 
 comp-deferred-compilation t
 ;; comp-deferred-compilation-black-list '()
 ;; or it will be too annoying
 comp-async-report-warnings-errors nil)
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))

(setq comp-deferred-compilation-deny-list ())
(setq straight-vc-git-default-clone-depth 1)

(setq straight-disable-native-compile
      (when (fboundp 'native-comp-available-p)
	(not (native-comp-available-p))))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default t)

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq use-package-verbose t)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

(setq package-enable-at-startup nil)

;; Find Executable Path on OS X
(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))
  )

;; Add my library path to load-path
(push "~/.dotfiles/Emacs/emacs-configs/my-emacs/lisp" load-path)

(push "~/Documents/Org" load-path)

;; 关闭备份
(setq make-backup-files nil
      auto-save-default nil)

(setq auto-window-vscroll nil)

(global-auto-revert-mode 1)

;; Since we don't want to disable org-confirm-babel-evaluate all
;; of the time, do it around the after-save-hook
(defun dw/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
    (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'dw/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode)))

(use-package super-save
  :defer 1
  :diminish super-save-mode
  :custom
  (super-save-auto-save-when-idle t)
  :config
  (super-save-mode +1))

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

(defun dw/set-font-faces ()
  (message "Setting faces!")
  ;; set font
  (set-face-attribute 'default nil :font "Victor Mono" :height 150)

  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font "Victor Mono" :height 150)

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
	 (global-ligature-mode t)) graphic-only-plugins-setting)

(use-package all-the-icons
  :custom
  (all-the-icons-dired-monochrome t))

(use-package doom-themes)

(if (not (display-graphic-p))
    (load-theme 'doom-one t))

(if (display-graphic-p)
    (defun dw/apply-theme (appearance)
      "Load theme, taking current system APPEARANCE into consideration."
      (mapc #'disable-theme custom-enabled-themes)
      (pcase appearance
	('light (load-theme 'doom-solarized-light t))
	('dark (load-theme 'doom-one t))))
  )

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-window-width-limit fill-column) 
  ;; (doom-modeline-major-mode-color-icon nil)
  )

(use-package dashboard
  :init
  ;; Set the title
  ;; (setq dashboard-banner-logo-title "Code Better, Live Longer!")
  ;; Set the banner
  (setq dashboard-startup-banner "~/.dotfiles/Emacs/dashboard/banner.txt")
  (setq dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 7)
                          (projects . 5)
                          ;; To display today’s agenda items on the dashboard
                          ;; (agenda . 5)
                          ))
  ;; To show agenda for the upcoming seven days
  ;; (setq dashboard-week-agenda t)
  ;; To customize which categories from the agenda items should be visible in the dashboard
  ;; (setq dashboard-org-agenda-categories '("Tasks" "Appointments"))
  ;; To show all agenda entries
  ;; (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)
  ;; To have an extra filter
  ;; (setq dashboard-match-agenda-entry nil)

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-set-init-info t)
  )

(push '(use-package nyan-mode
	 :config
	 (setq nyan-mode t)
	 :custom
	 (nyan-animate-nyancat t)
	 (nyan-wavy-trail t)
	 ) graphic-only-plugins-setting)

(use-package hl-todo
  :defer t
  :hook ((org-mode lsp-mode) . hl-todo-mode)
  :config
  (setq hl-todo-keyword-faces
      '(("TODO"   . "#FF0000")
        ("FIXME"  . "#FF0000")
        ("DEBUG"  . "#A020F0")
        ("NEXT" . "#FF4500")
        ("UNCHECK"   . "#1E90FF")))
  )

(use-package highlight-numbers
  :hook ((org-mode lsp-mode) . highlight-numbers-mode))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (setq doom-modeline-icon t)
		;; (load-theme 'doom-one t)
		(add-hook 'ns-system-appearance-change-functions #'dw/apply-theme)
		(dashboard-setup-startup-hook)
                (with-selected-frame frame
                  (dw/set-font-faces))
		(setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))
		))
  ;; (load-theme 'doom-one t)
  ;; (lab-themes-load-style 'dark)
  (add-hook 'ns-system-appearance-change-functions #'dw/apply-theme)
  (dw/set-font-faces)
  )

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package edwina
  :config
  (setq display-buffer-base-action '(display-buffer-below-selected))
  ;; (edwina-setup-dwm-keys)
  (edwina-mode 1))

(use-package dired
  :ensure nil
  :straight nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  ;;:config
  ;;(evil-collection-define-key 'normal 'dired-mode-map
  ;;  "d" 'dired-single-up-directory
  ;;  "n" 'dired-single-buffer)
  )

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  ;;:config
  ;;(evil-collection-define-key 'normal 'dired-mode-map
  ;;  "H" 'dired-hide-dotfiles-mode)
  )

(use-package diredfl
  :hook (dired-mode . diredfl-mode)
  )

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/Projects/Code")
    (setq projectile-project-search-path '("~/Documents/Projects/Code")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("C-l" . ivy-alt-done)
         ("C-n" . ivy-next-line)
         ("C-p" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-p" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-p" . ivy-previous-line)
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
  (setq ivy-initial-inputs-alist nil) ;; Don't start searches with ^
  )

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package ivy-posframe
 :after ivy
 :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (ivy-posframe-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun dw/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
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

  (require 'init-org-agenda)

  (use-package ob-browser)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (latex . t)
     (java . t)
     (C . t)
     (js . t)
     (css . t)
     (browser . t)
     (R . t)
     (ditaa . t)
     (python . t)))

  (setq org-confirm-babel-evaluate nil)
  (push '("conf-unix" . conf-unix) org-src-lang-modes)

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
            'dw/org-html-wrap-blocks-in-code)
  )

;; change bullets for headings
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen(-) with dot(.)
;; (font-lock-add-keywords 'org-mode
;;                         '(("^ *\\([-]\\) "
;;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(with-eval-after-load 'org
  ;; Make sure org faces is available
  (require 'org-faces)
  ;; Make sure org-indent face is available
  (require 'org-indent)
  ;; Set Size and Fonts for Headings
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.0)
                  (org-level-6 . 1.0)
                  (org-level-7 . 1.0)
                  (org-level-8 . 1.0)))
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
  )

;; This is needed as of Org 9.2
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
  (add-to-list 'org-structure-template-alist '("d" . "src ditaa :file ../images/.png :cmdline -E"))
 )

(defun dw/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . dw/org-mode-visual-fill))

(use-package org-download
	  :ensure t 
	  ;;将截屏功能绑定到快捷键：Ctrl + Shift + Y
	  :bind ("C-S-y" . org-download-screenshot)
	  :config
	  (require 'org-download)
	  ;; Drag and drop to Dired
	  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package valign
  :hook (org-mode . valign-mode))

(use-package markdown-mode
 :ensure t
 :mode ("README\\.md\\'" . gfm-mode)
 :init (setq markdown-command "multimarkdown"))

(use-package edit-indirect
  :after markdown-mode)

;; For DVP
;; (require 'init-meow-dvp)

;; For Qwerty
(require 'init-meow-qwerty)

(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :config
  ;; meow-setup 用于自定义按键绑定，可以直接使用下文中的示例
  (meow-setup)
  ;; 如果你需要在 NORMAL 下使用相对行号（基于 display-line-numbers-mode）
  (meow-setup-line-number)
  ;;(add-to-list 'meow-normal-state-mode-list 'dashboard-mode)
  ;; (setq meow-replace-state-name-list
  ;; '((normal . "Ꮚ•ꈊ•Ꮚ")
  ;;   (insert . "Ꮚ`ꈊ´Ꮚ")
  ;;   (keypad . "Ꮚ'ꈊ'Ꮚ")
  ;;   (motion . "Ꮚ-ꈊ-Ꮚ")))
  :bind ("C-g" . meow-insert-exit)
  )

(meow-leader-define-key
 '("f" . find-file)
 '("b" . counsel-switch-buffer)
 '("t" . vterm-toggle)
 '("qr" . quickrun)
 '("oo" . ace-window)
 '("od" . ace-delete-window)
 '("dd" . dap-debug)
 '("aa" . org-agenda)
 '("al" . org-agenda-list)
 '("ac" . org-capture)
)

(meow-motion-overwrite-define-key
 '("h" . dired-single-up-directory)
 '("l" . dired-single-buffer))

;; set delete selection mode
(delete-selection-mode t)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package color-rg
  :straight (color-rg :type git :host github :repo "manateelazycat/color-rg")
  :commands (color-rg-search-input
             color-rg-search-symbol
             color-rg-search-input-in-project
             color-rg-search-input-in-current-file
             color-rg-search-project-with-typ)
  )

;; (dw/leader-key-def
;;   "c" '(:ignore t :which-key "color-rg")
;;   "cid" 'color-rg-search-input
;;   "csd" 'color-rg-search-symbol
;;   "cip" 'color-rg-search-input-in-project
;;   "cic" 'color-rg-search-input-in-current-file
;;   "cit" 'color-rg-search-project-with-type)

(use-package multiple-cursors
  :commands (mc/edit-lines mc/mark-next-like-this mc/mark-previous-like-this mc/mark-all-like-this)
  :bind
  (("C-S-c C-S-c" . 'mc/edit-lines)
   ("C->" . 'mc/mark-next-like-this)
   ("C-<" . 'mc/mark-previous-like-this)
   ("C-S-c C-<" . 'mc/mark-all-like-this)))

(use-package iedit
  :after lsp)

(use-package evil-nerd-commenter
  :defer t
  :bind
  ("M-;" . 'evilnc-comment-or-uncomment-lines)
  ("C-c l" . 'evilnc-quick-comment-or-uncomment-to-the-line)
  ("C-c c" . 'evilnc-copy-and-comment-lines)
  ("C-c p" . 'evilnc-comment-or-uncomment-paragraphs)
  ;; :config
  ;; (evilnc-default-hotkeys t)
  )

(use-package company 
  :hook (lsp-mode . company-mode)
  ;; :bind 
  ;; (:map company-active-map
  ;;       ("<tab>". company-complete-selection))
  ;; (:map lsp-mode-map
  ;;       ("<tab>" . company-indent-or-complete-common)
  ;;       ("<M-n>" . company-select-next-or-abort)
  ;;       ("<M-p>" . company-select-previous-or-abort))
  :custom
  (company-tooltip-align-annotations t)
  ;; Number the candidates (use M-1, M-2 etc to select completions)
  (company-show-numbers t)
  ;; starts with 1 character
  (company-minimum-prefix-length 1)
  ;; Trigger completion immediately
  (company-idle-delay 0)
  ;; Back to top when reach the end
  (company-selection-wrap-around t)
  :config
  ;; (setq global-company-mode t)
  ;; Use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  (company-tng-configure-default)
  ;;Completion based on AI
  ;; (use-package company-tabnine
  ;;   :config
  ;;   (push '(company-capf :with company-tabnine :separate company-yasnippet :separete) company-backends))
  (push '(company-capf :with company-yasnippet :separate) company-backends)
  )

;; Add UI for Company
(use-package company-box
  :hook (company-mode . company-box-mode)
  :config
  (setq company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package smartparens
  :hook (lsp-mode . smartparens-mode))

(use-package rainbow-delimiters
  :hook (lsp-mode . rainbow-delimiters-mode))

(use-package hungry-delete
  :hook (lsp-mode . hungry-delete-mode))

(use-package indent-guide
  :hook (lsp-mode . indent-guide-mode))

(use-package format-all
  :hook (lsp-mode . format-all-mode)
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

;; Set up Keybindings
;; (dw/leader-key-def
;;   "r"  '(:ignore t :which-key "quickrun")
;;   "rr" 'quickrun
;;   "ra" 'quickrun-with-arg
;;   "rs" 'quickrun-shell
;;   "rc" 'quickrun-compile-only
;;   "re" 'quickrun-region)

(use-package flycheck
  :hook (lsp-mode . flycheck-mode))

(use-package yasnippet
  :hook ((lsp-mode org-mode) . yas-minor-mode)
  :config
  (setq yas-snippet-dirs
    '("~/.dotfiles/Emacs/snippets"))
  (yas-reload-all))

;; Snippets Collection
(use-package yasnippet-snippets
  :after yasnippet)

;; auto insert
(use-package auto-yasnippet
  :after yasnippet)

;; (dw/leader-key-def
;;   "a"  '(:ignore t :which-key "auto-snippets")
;;   "aw" 'aya-create
;;   "ay" 'aya-expand
;;   "ao" 'aya-open-line)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (
         ((sh-mode typescript-mode js-mode web-mode python-mode css-mode Latex-mode TeX-latex-mode c-mode cc-mode) . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-signature-auto-activate nil)
  (lsp-signature-render-documentation nil)
  (lsp-log-io nil)
  (lsp-idle-delay 0.500)
  (lsp-completion-provider :capf)
  :config
  (add-to-list 'lsp-language-id-configuration '((scss-mode . "css")
                                                (less-css-mode . "css")))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-imenu-auto-refresh t)
  )

(use-package lsp-ivy 
  :after lsp
  :commands lsp-ivy-workspace-symbol)
(use-package lsp-treemacs
  :after lsp
  :commands lsp-treemacs-errors-list)

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package pyenv-mode
  :hook (python-mode . pyenv-mode)
  :config
  ;; auto activates the virtual environment if .python-version exists
  (use-package pyenv-mode-auto)
  ) 


;; (dw/leader-key-def
;;   "p"  '(:ignore t :which-key "pyenv")
;;   "pp" 'pyenv-mode
;;   "ps" 'pyenv-mode-set
;;   "pu" 'pyenv-mode-unset
;;   "pr" 'run-python)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'")

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

(use-package coffee-mode
  :mode "\\.coffee\\'"
  :config
  ;; automatically clean up bad whitespace
  (setq whitespace-action '(auto-cleanup))
  ;; This gives you a tab of 2 spaces
  (custom-set-variables '(coffee-tab-width 2))
  
  (use-package sourcemap)
  ;; generating sourcemap by '-m' option. And you must set '--no-header' option
  (setq coffee-args-compile '("-c" "--no-header" "-m"))
  (add-hook 'coffee-after-compile-hook 'sourcemap-goto-corresponding-point)

  ;; If you want to remove sourcemap file after jumping corresponding point
  (defun my/coffee-after-compile-hook (props)
    (sourcemap-goto-corresponding-point props)
    (delete-file (plist-get props :sourcemap)))
  (add-hook 'coffee-after-compile-hook 'my/coffee-after-compile-hook)
  )

(use-package flymake-coffee
  :hook (coffee-mode . flymake-coffee)
  )

(use-package web-mode
  :mode "\\.\\(html?\\|ejs\\|tsx\\|jsx\\)\\'")

;; Impatient Html File
;; (use-package impatient-mode
;;   :after web-mode)

;; Preview the html file
(use-package skewer-mode
  :after web-mode
  :config
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode)
  (add-hook 'web-mode-hook 'skewer-html-mode))

(use-package emmet-mode
  :hook (web-mode . emmet-mode))

(use-package scss-mode
  :mode "\\.scss\\'"
  :custom
  (scss-compile-at-save t)
  (scss-output-directory "../css")
  (scss-sass-command "sass --no-source-map")
  )

(use-package latex-preview-pane
  :ensure t
  :after (tex-mode Latex-mode latex-mode TeX-latex-mode))

(straight-use-package 'auctex)

(use-package cdlatex
  :hook 
  (org-mode . org-cdlatex-mode)
  (LaTeX-mode . cdlatex-mode)
  (latex-mode . cdlatex-mode)
  )

(use-package lsp-sourcekit
  :after swift-mode
  :config
  (setq lsp-sourcekit-executable "/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/bin/sourcekit-lsp"))

(use-package swift-mode
  :mode "\\.swift\\'"
  :hook (swift-mode . (lambda () (lsp))))

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package json-mode
  :mode "\\.json\\'")

;; dap debug tools
(use-package dap-mode
  :commands dap-debug 
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed

  (require 'dap-python)
  (setq dap-auto-configure-features '(sessions locals controls tooltip)))

(use-package vterm
  :commands vterm
  :config
  ;; (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

(use-package multi-vterm
  :after vterm)

(use-package vterm-toggle
  :after vterm)

(use-package magit
  :commands (magit-status magit-get-current-branch)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Add a super-convenient global binding for magit-status since
;; I use it 8 million times a day
(global-set-key (kbd "C-M-;") 'magit-status)

(use-package leetcode
  :commands (leetcode start-leetcode)
  :custom
  (leetcode-prefer-language "python3")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  (leetcode-directory "~/Documents/leetcode")
  )

(defun start-leetcode()
    (interactive)
    (global-display-line-numbers-mode -1)
    (display-line-numbers-mode -1)
    (leetcode)
    )

(defun quit-leetcode()
  (interactive)
  (leetcode-quit)
  (global-line-numebrs-mode t)
  )

(setq gc-cons-threshold 100000000)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(if (display-graphic-p)
    (dolist (elisp-code graphic-only-plugins-setting)
      (eval elisp-code)))
