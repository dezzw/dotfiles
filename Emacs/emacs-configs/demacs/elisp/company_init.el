(require 'company)

(defun set-company-tab ()
  (define-key company-active-map [tab] 'company-select-next-if-tooltip-visible-or-complete-selection)
  (define-key company-active-map (kbd "TAB") 'company-select-next-if-tooltip-visible-or-complete-selection)
  )

(set-company-tab)

(define-key company-active-map (kbd "<backtab>") 'company-select-previous)
(define-key company-active-map (kbd "S-TAB") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates)
(setq company-show-numbers t)

(setq company-frontends
      '(company-pseudo-tooltip-unless-just-one-frontend
        company-preview-frontend
        company-echo-metadata-frontend))

(add-hook 'company-mode-hook '(lambda ()
                                (setq company-backends
                                      (delete 'company-bbdb
                                              (delete 'company-oddmuse
                                                      (delete 'company-cmake
                                                              (delete 'company-clang company-backends)))))
                                ))

(defun advice-only-show-tooltip-when-invoked (orig-fun command)
  "原始的 company-pseudo-tooltip-unless-just-one-frontend-with-delay, 它一直会显示
candidates tooltip, 除非只有一个候选结果时，此时，它会不显示, 这个 advice 则是让其
完全不显示, 但是同时仍旧保持 inline 提示, 类似于 auto-complete 当中, 设定
ac-auto-show-menu 为 nil 的情形, 这种模式比较适合在 yasnippet 正在 expanding 时使用。"
  (when (company-explicit-action-p)
    (apply orig-fun command)))

(defun advice-always-trigger-yas (orig-fun &rest command)
  (interactive)
  (unless (ignore-errors (yas-expand))
    (apply orig-fun command)))

(with-eval-after-load 'yasnippet
  (defun yas/disable-company-tooltip ()
    (interactive)
    (advice-add #'company-pseudo-tooltip-unless-just-one-frontend :around #'advice-only-show-tooltip-when-invoked)
    (define-key company-active-map [tab] 'yas-next-field-or-maybe-expand)
    (define-key company-active-map (kbd "TAB") 'yas-next-field-or-maybe-expand)
    )
  (defun yas/restore-company-tooltip ()
    (interactive)
    (advice-remove #'company-pseudo-tooltip-unless-just-one-frontend #'advice-only-show-tooltip-when-invoked)
    (set-company-tab)
    )
  (add-hook 'yas-before-expand-snippet-hook 'yas/disable-company-tooltip)
  (add-hook 'yas-after-exit-snippet-hook 'yas/restore-company-tooltip)

  ;; 这个可以确保，如果当前 key 是一个 snippet, 则一定展开 snippet,
  ;; 而忽略掉正常的 company 完成。
  (advice-add #'company-select-next-if-tooltip-visible-or-complete-selection :around #'advice-always-trigger-yas)
  (advice-add #'company-complete-common :around #'advice-always-trigger-yas)
  (advice-add #'company-complete-common-or-cycle :around #'advice-always-trigger-yas)
  )


(setq company-auto-commit t)
;; 32 空格, 41 右圆括号, 46 是 dot 字符
;; 这里我们移除空格，添加逗号(44), 分号(59)
;; 注意： C-x = 用来检测光标下字符的数字，(insert 数字) 用来测试数字对应的字符。
(setq company-auto-commit-chars '(41 46 44 59))

(add-hook 'after-init-hook 'global-company-mode)

(provide 'company_init)

;;; company_init.el ends here
