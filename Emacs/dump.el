;; 先将Emacs加载一下。注意这时候init.el执行时的逻辑应该是非dump方式的代码。
;; 这样也就将你的原本配置中的插件都加载到内存中了，随后就可以设置dump插件黑名单了。
(load (expand-file-name "init.el"))

;; 这里就是不需要dump的插件，也就是黑名单。
(setq +dump-exclude-packages '(meow))

;; 这里是加载配置的过程，作用将所有已安装的插件都加载到内容，但是会排除黑名单中的插件。
(dolist (package package-activated-list)
  (unless (member package +dump-exclude-packages)
    (require package)))

;; 现在需要创建一个变量，其作用是让init.el里有两种不同启动时加载逻辑以及解决dump带来的load-path的问题
(setq +dumped-load-path load-path)

;; 关闭gc，可选
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; 真正做dump工作的函数，你可以自定义生成的文件的路径
(dump-emacs-portable "~/.emacs.d/emacs.pdmp")
