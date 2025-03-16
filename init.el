;; 确保包管理器已配置 MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; 使用 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Evil 配置
(use-package evil
  :init
  (setq evil-want-keybinding nil)  ;; 避免警告消息
  (setq evil-jumps-cross-buffers t)
  (setq evil-jumps-max-length 300)
  (setq evil-jumps-pre-jump-hook nil)
  :config
  (evil-mode 1))

;; Projectile 配置
(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :bind
  ("C-r" . projectile-switch-project)  ;; 类似 VSCode 的 Ctrl+R
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching t))
;; Projectile 集成 counsel 提供更好的补全体验
(use-package counsel-projectile
  :after projectile
  :config
  (counsel-projectile-mode 1))
;; Ivy 配置
(use-package ivy
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t))
(use-package counsel
  :after ivy
  :config
  (counsel-mode 1))
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Ripgrep 配置
(use-package rg
  :defer t
  :init
  (rg-enable-default-bindings)  ;; 使用默认键绑定 C-c s
  :config
  (setq rg-group-result t)      ;; 按文件分组显示结果
  (setq rg-hide-command t)      ;; 隐藏命令行
  (setq rg-show-columns nil)    ;; 不显示列号
  :bind
  (("C-c g" . rg-dwim)         ;; 快速搜索当前单词
   ("C-c G" . rg)))            ;; 打开交互式搜索界面

;; 终端
(use-package vterm
  :defer t)  ;; 延迟加载，直到实际需要时

;; 主题
(use-package doom-themes
  :config
  (load-theme 'doom-one t))

;; LSP 配置
(use-package lsp-mode
  :defer t
  :hook (rustic-mode . lsp)
  :commands lsp
  :config
  (setq lsp-idle-delay 0.5
        lsp-log-io nil  ;; 禁用日志记录以提高性能
        lsp-completion-provider :capf
        lsp-prefer-flymake nil))

;; Rust 开发配置
(use-package rustic
  :defer t
  :config
  (setq rustic-format-on-save t))

;; LSP UI 配置
(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t))

;; 减少 GC 频率
(setq gc-cons-threshold 100000000)  ;; 100MB
(setq read-process-output-max (* 1024 1024))  ;; 1MB

;; 重新加载配置文件的快捷键
(defun reload-init-file ()
  "Reload init.el file"
  (interactive)
  (load-file user-init-file)
  (message "Reloaded init.el"))
(global-set-key (kbd "C-c r") 'reload-init-file)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel-projectile doom-themes evil ivy-rich lsp-ui projectile
			rustic vterm)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
  ;; 140 对应 14pt 大小，你可以根据需要调整这个值

;; 设置中文字体（可选）
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (font-spec :family "PingFang SC"
                              :size 14)))

;; 显示当前字体大小的快捷命令
(defun show-font-size ()
  "Show current font size in points."
  (interactive)
  (let ((height (face-attribute 'default :height)))
    (message "当前字体大小: %d (%dpt)" height (/ height 10))))
;; 绑定到快捷键 C-c f s
(global-set-key (kbd "C-c f s") 'show-font-size)
