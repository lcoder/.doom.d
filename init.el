;; 确保包管理器已配置 MELPA
(use-package package
  :ensure nil
  :config
  (package-initialize)
  :custom
  (package-native-compile t)
  (package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

;; 显示行号配置
(use-package display-line-numbers
  ;; 内置包，不需要安装
  :ensure nil
  :config
  ;; 设置行号类型（绝对行号）
  (setq display-line-numbers-type t)
  ;; 全局启用
  (global-display-line-numbers-mode t))

;; Evil 配置
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  ;; 在 evil normal 状态下绑定lsp-bridge相关快捷键
  (evil-define-key 'normal 'global "K" 'lsp-bridge-popup-documentation)
  (define-key evil-motion-state-map "gR" #'lsp-bridge-rename)
  (define-key evil-motion-state-map "gr" #'lsp-bridge-find-references)
  (define-key evil-normal-state-map "gi" #'lsp-bridge-find-impl)
  (define-key evil-motion-state-map "gd" #'lsp-bridge-find-def)  ;; 修正：从gd->jump改为gd->find-def
  (define-key evil-motion-state-map "gs" #'lsp-bridge-restart-process)
  (define-key evil-normal-state-map "gh" #'lsp-bridge-popup-documentation)
  (define-key evil-normal-state-map "gn" #'lsp-bridge-diagnostic-jump-next)
  (define-key evil-normal-state-map "gp" #'lsp-bridge-diagnostic-jump-prev)
  (define-key evil-normal-state-map "ga" #'lsp-bridge-code-action)
  (define-key evil-normal-state-map "ge" #'lsp-bridge-diagnostic-list))
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

(use-package undo-tree
  :ensure t
  :after evil
  :diminish
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;; Treemacs 配置
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window nil)
  (setq treemacs-project-follow-mode t)
  (setq treemacs-follow-mode t)
  (setq treemacs-filewatch-mode t)
  (setq treemacs-fringe-indicator-mode 'always)
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t t"   . treemacs)))

;; Evil 用户的 Treemacs 集成
(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

;; Projectile 配置
(use-package projectile
  :init
  (projectile-mode +1)
  :bind-keymap
  ("s-p" . projectile-command-map)
  :config
  ;; 确保使用默认的completion系统，与ivy兼容
  (setq projectile-completion-system 'ivy)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-enable-caching t)
  ;; 确保projectile知道保存的项目列表
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))
  (setq projectile-auto-save-known-projects t)
  ;; 如果已有项目文件，确保加载它
  (when (file-exists-p projectile-known-projects-file)
    (projectile-load-known-projects)))
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
(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))
(use-package counsel
  :after ivy
  :config
  (counsel-mode 1))

;; 终端
(use-package vterm
  :defer t)  ;; 延迟加载，直到实际需要时

;; 主题
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))
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

;; markdown 配置
(use-package markdown-mode
  :ensure t
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
         ("C-c C-e" . markdown-do)))

;; lsp-bridge 配置
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(add-to-list 'load-path "~/.emacs.d/plugins/lsp-bridge")
;; 设置lsp-bridge使用虚拟环境
(setq lsp-bridge-python-command "~/.emacs.d/lsp-bridge-venv/bin/python3")

(require 'yasnippet)
(yas-global-mode 1)
(require 'lsp-bridge)
(global-lsp-bridge-mode)

;; 重新加载配置文件的快捷键
(defun reload-init-file ()
  "Reload init.el file"
  (interactive)
  (load-file user-init-file)
  (message "Reloaded init.el"))
(global-set-key (kbd "C-c r") 'reload-init-file)

;; 关闭滚动条
(scroll-bar-mode -1)

(defun load-font-setup ()
  (let ((emacs-font-size 14)
        (english-font-name "Iosevka Nerd Font")
        (chinese-font-name "Sarasa Fixed SC"))
    (set-face-attribute 'default nil :family english-font-name :height (* emacs-font-size 10))
    (set-fontset-font t 'han (font-spec :family chinese-font-name))
    (set-fontset-font t 'cjk-misc (font-spec :family chinese-font-name))
    (set-fontset-font t 'bopomofo (font-spec :family chinese-font-name))
    (set-fontset-font t 'kana (font-spec :family chinese-font-name))
    (set-fontset-font t 'symbol (font-spec :family chinese-font-name))))
(load-font-setup)

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

;; Org mode 配置
(use-package org
  :ensure t
  :config
  ;; 禁用行号显示
  (setq display-line-numbers nil)
  (setq org-startup-indented t)           ; 启用缩进模式
  (setq org-startup-folded t)             ; 默认折叠所有标题
  (setq org-log-done 'time)               ; 记录 TODO 完成时间
  (setq org-agenda-files '("~/org"))      ; 设置议程文件目录
  ;; 设置代码块高亮
  (setq org-src-fontify-natively t)       ; 启用代码块语法高亮
  (setq org-src-tab-acts-natively t)      ; 在代码块中使用语言的缩进规则
  (setq org-edit-src-content-indentation 0) ; 代码块缩进从0开始
  (setq org-src-preserve-indentation t)   ; 保持代码块的原始缩进
  ;; 设置代码块的默认参数，确保每次执行时都替换结果
  (setq org-babel-default-header-args
        '((:results . "replace")
          (:exports . "both")
          (:session . "none")))
  ;; 设置 Rust 代码块的缩进为 2 个空格
  (setq rust-indent-offset 2)
  ;; TODO 工作流状态
  (setq org-todo-keywords
        '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  ;; 配置 org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (sql . t)
     (js . t)
     (rust . t)))
   (setq org-confirm-babel-evaluate nil)   ; 不询问是否执行代码块
    ;; 设置 Rust 代码块的默认参数
    (setq org-babel-default-header-args:rust '((:results . "output")
                                              (:session . "*Rust*")
                                              (:exports . "both")))
    ;; 添加调试信息
    (setq org-babel-rust-command "rust-script")
    (setq org-babel-rust-command-args '("--debug"))

    ;; 添加环境变量设置函数
    (defun my/org-babel-set-env-vars (orig-fun &rest args)
      "在执行代码块前设置环境变量"
      (let ((process-environment (copy-sequence process-environment)))
        (push "RUST_BACKTRACE=full" process-environment)
        (apply orig-fun args)))
    
    ;; 在执行代码块前自动设置环境变量
    (advice-add 'org-babel-execute-src-block :around #'my/org-babel-set-env-vars)
  ;; 设置省略号样式和标题对齐
  (setq org-ellipsis " ⤵ ")
  (set-face-attribute 'org-ellipsis nil :foreground "#E6DC88")
  (set-face-attribute 'org-level-1 nil :height 1.2 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.1 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.0 :weight 'bold)
  (set-face-attribute 'org-level-4 nil :height 1.0 :weight 'bold)
  ;; 你可以根据需要继续设置 org-level-5 等
  :bind
  (("C-c a" . org-agenda)                 ; 打开议程视图
   ("C-c c" . org-capture)                ; 快速捕获
   ("C-c l" . org-store-link)             ; 存储链接
   ("C-c C-," . org-insert-structure-template)  ; 插入代码块
   ("C-c C-'" . org-edit-special)         ; 编辑代码块
   ("C-c C-c" . org-babel-execute-src-block))) ; 执行代码块

;; 添加 evil-org 配置
(use-package evil-org
  :ensure t
  :after (evil org)
  :hook (org-mode . evil-org-mode)
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  ;; 修复 TAB 键行为
  (evil-define-key '(normal insert) evil-org-mode-map
    (kbd "<tab>") 'org-cycle
    (kbd "TAB") 'org-cycle))

;; 开启自动折行
(setq truncate-lines nil)

;; 词典
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(bing-dict company counsel-projectile default-text-scale
	       doom-themes emojify evil-collection evil-embrace
	       evil-escape evil-org evil-textobj-entire ivy-rich
	       lsp-ui rg rustic sdcv transpose-frame
	       treemacs-all-the-icons treemacs-evil
	       treemacs-projectile undo-tree valign vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
