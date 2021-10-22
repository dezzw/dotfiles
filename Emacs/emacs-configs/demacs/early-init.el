(setq gc-cons-threshold most-positive-fixnum)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq load-prefer-newer noninteractive)

;; Add my library path to load-path
(push "~/.dotfiles/Emacs/emacs-configs/demacs/elisp" load-path)

(setq comp-async-jobs-number 7
      comp-deferred-compilation t
	 comp-async-report-warnings-errors nil)
(add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))


(require 'init-package)
(require 'init-gc)

(require 'init-path)

;; 关闭备份
(setq make-backup-files nil
     auto-save-default nil)

(setq auto-window-vscroll nil)

(global-auto-revert-mode 1)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)

;; (menu-bar-mode -1)          ; Disable the menu bar

(menu-bar-mode t)

(setq ring-bell-function 'ignore)

(set-frame-parameter (selected-frame) 'alpha '(90 . 90))
(add-to-list 'default-frame-alist '(alpha . (90 . 90)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;; early-init.el ends here
