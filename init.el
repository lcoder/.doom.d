;; 确保包管理器已配置 MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; 使用 use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; 显示行号配置
(use-package display-line-numbers
  ;; 内置包，不需要安装
  :ensure nil
  :config
  ;; 设置行号类型（绝对行号）
  (setq display-line-numbers-type t)
  ;; 全局启用
  (global-display-line-numbers-mode t)
  ;; 在某些模式下禁用行号
  (dolist (mode '(org-mode-hook
                  vterm-mode-hook
                  treemacs-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

;; Evil 配置
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; surround 支持
(use-package evil-surround
  :ensure t
  :after evil
  :config
  ;; 启用全局的 evil-surround 模式
  (global-evil-surround-mode 1))

;; surround 增强
(use-package evil-embrace
  :ensure t
  :after (evil evil-surround)
  :config
  ;; 启用 evil-surround 和 embrace 的集成
  (evil-embrace-enable-evil-surround-integration)

  ;; 禁用帮助提示（可选，如果觉得提示窗口烦人）
  ; (setq evil-embrace-show-help-p nil)

  ;; 为 Org-mode 添加自定义环绕对
  (defun my/org-mode-embrace-hook ()
    "Add Org-mode specific surrounding pairs."
    (embrace-add-pair ?* "*" "*")  ;; 加粗
    (embrace-add-pair ?/ "/" "/")  ;; 斜体
    (embrace-add-pair ?~ "~" "~")) ;; 代码
  (add-hook 'org-mode-hook 'my/org-mode-embrace-hook))

;; jk 退出插入模式
(use-package evil-escape
  :ensure t
  :after evil
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-key-sequence "jk")
  (setq-default evil-escape-delay 0.2))

;; Treemacs 配置
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t)
  (setq treemacs-width 30)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t b"   . treemacs-bookmark)
        ("C-x t d"   . treemacs-select-directory)))
;; Evil 用户的 Treemacs 集成
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)
;; Projectile 集成
(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; Projectile 配置
(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("s-p" . projectile-command-map)
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
  :ensure t
  :config
  (load-theme 'doom-one-light t))
;; 定义函数在亮暗主题之间切换
(defun toggle-dark-light-theme ()
  "Toggle between dark and light themes."
  (interactive)
  (if (eq (car custom-enabled-themes) 'doom-one)
      (progn
        (disable-theme 'doom-one)
        (load-theme 'doom-one-light t))
    (disable-theme 'doom-one-light)
    (load-theme 'doom-one t)))
;; 绑定到快捷键 (例如 F5)
(global-set-key (kbd "<f5>") 'toggle-dark-light-theme)

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

;; 启用原生编译
(setq package-native-compile t)
(setq native-comp-async-report-warnings-errors nil)

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

;; 关闭滚动条
(scroll-bar-mode -1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(all-the-icons bing-dict counsel-projectile doom-themes evil-embrace
		   evil-escape evil-org evil-surround
		   evil-textobj-entire ivy-rich lsp-ui rg rustic sdcv
		   transpose-frame treemacs treemacs-all-the-icons
		   treemacs-evil treemacs-projectile vterm)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; 删除 cnfonts 配置，替换为 cnfonts 配置
(use-package cnfonts
  :ensure t
  :config
  (cnfonts-mode 1)
  ;; 添加两个字号增减的快捷键
  :bind
  (("s-=" . cnfonts-increase-fontsize)  ; Command + =
   ("s-+" . cnfonts-increase-fontsize)  ; Command + Shift + =
   ("s--" . cnfonts-decrease-fontsize)  ; Command + -
   ("s-0" . cnfonts-set-default-fontsize)))   ; Command + 0

;; 显示当前字体大小的快捷命令
(defun show-font-size ()
  "Show current font size in points."
  (interactive)
  (let ((height (face-attribute 'default :height)))
    (message "当前字体大小: %d (%dpt)" height (/ height 10))))
;; 绑定到快捷键 C-c f s
(global-set-key (kbd "C-c f s") 'show-font-size)

;; 启用内置 which-key
(require 'which-key)
(which-key-mode 1)
;; 可选的一些自定义配置
(setq which-key-idle-delay 0.5)                     ;; 显示延迟时间（秒）
(setq which-key-idle-secondary-delay 0.05)          ;; 后续按键的延迟
(setq which-key-side-window-max-height 0.3)         ;; 窗口最大高度（屏幕高度的比例）
(setq which-key-sort-order 'which-key-key-order)    ;; 按键排序方式


;; Org mode 配置
(use-package org
  :ensure t
  :config
  (setq org-startup-indented t)           ; 启用缩进模式
  (setq org-startup-folded t)             ; 默认折叠所有标题
  (setq org-log-done 'time)               ; 记录 TODO 完成时间
  (setq org-agenda-files '("~/org"))      ; 设置议程文件目录
  ;; TODO 工作流状态
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  :bind
  (("C-c a" . org-agenda)                 ; 打开议程视图
   ("C-c c" . org-capture)                ; 快速捕获
   ("C-c l" . org-store-link)))           ; 存储链接

;; org-modern SF Mono
;; ellipsis https://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(use-package org
  :ensure t
  :custom
  (org-ellipsis " ⤵ ")
  :custom-face
  (org-ellipsis ((t (:foreground "#E6DC88" :underline nil))))
  :config
  (add-hook 'org-mode-hook (lambda ()
                            ;; 使用更持久的字体设置方式
                            (set-face-attribute 'org-default nil :family "SF Mono")
                            (buffer-face-set '(:family "SF Mono"))
                            ;; 确保 org-ellipsis 的设置生效
                            (set-face-attribute 'org-ellipsis nil
                                              :foreground "#E6DC88"
                                              :underline nil))))

;; 添加 evil-org 配置
(use-package evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ;; 添加键位主题设置
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar return))
  ;; 修复 TAB 键行为
  (evil-define-key '(normal insert) evil-org-mode-map
    (kbd "<tab>") 'org-cycle
    (kbd "TAB") 'org-cycle))

(use-package sdcv
  :ensure t
  :bind (("C-c d" . sdcv-search-input)
         ("C-c i" . sdcv-search-pointer+)
         ("C-c p" . sdcv-search-pointer))
  :config
  (setq sdcv-dictionary-simple-list '("XDICT英汉辞典"))
  (setq sdcv-dictionary-complete-list '("牛津英汉双解美化版")))

(use-package bing-dict
     :ensure t
     :bind ("C-c b" . bing-dict-brief))