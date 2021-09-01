(straight-use-package 'dashboard)

(require 'dashboard)

(setq dashboard-center-content t)
(setq dashboard-set-init-info t)

(dashboard-setup-startup-hook)

(provide 'init-dashboard)
