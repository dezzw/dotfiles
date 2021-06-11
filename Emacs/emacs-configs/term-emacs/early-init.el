(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer noninteractive)

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)

(menu-bar-mode -1)          ; Disable the menu bar


(setq ring-bell-function 'ignore)
