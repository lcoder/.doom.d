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
  ;; 设置行号右对齐
  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start t)
  ;; 全局启用
  (global-display-line-numbers-mode t))

;; Evil 配置
(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
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

;; 下划线作为单词字符
(defadvice evil-inner-word (around underscore-as-word activate)
  (let ((table (copy-syntax-table (syntax-table))))
    (modify-syntax-entry ?_ "w" table)
    (with-syntax-table table
      ad-do-it)))

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
  ;; 重新加载后，对所有已打开的 org-mode 缓冲区重新关闭行号显示
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (display-line-numbers-mode -1))))
  (message "Reloaded init.el"))
(global-set-key (kbd "C-c r") 'reload-init-file)

;; 关闭滚动条
(scroll-bar-mode -1)

(defun load-font-setup ()
  (let ((emacs-font-size 14)
        ;; (english-font-name "Iosevka Nerd Font")
        ;; (chinese-font-name "Sarasa Fixed SC"))
        ;; (english-font-name "Sarasa Term SC Nerd")
        ;; (chinese-font-name "Sarasa Term SC Nerd")
        (english-font-name "Sarasa Term SC Nerd")
        (chinese-font-name "Sarasa Term SC Nerd"))
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
  :hook (org-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (setq org-startup-indented t)           ; 启用缩进模式
  (setq org-startup-folded t)             ; 默认折叠所有标题
  (setq org-log-done 'time)               ; 记录 TODO 完成时间
  (setq org-agenda-files '("~/Library/Mobile Documents/com~apple~CloudDocs/org"))      ; 设置议程文件目录
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
  
  ;; ===== 新增：快速打开 org 文件的功能 =====
  
  ;; 定义 org 文件目录
  (defvar my/org-directory "~/Library/Mobile Documents/com~apple~CloudDocs/org"
    "Org 文件目录路径")
  
  ;; 快速打开 org 文件的函数
  (defun my/open-org-file ()
    "快速选择并打开 org 目录下的 org 文件"
    (interactive)
    (let ((default-directory (expand-file-name my/org-directory)))
      (if (file-exists-p default-directory)
          (counsel-find-file)
        (message "Org 目录不存在: %s" my/org-directory))))
  
  ;; 列出所有 org 文件的函数
  (defun my/list-org-files ()
    "列出 org 目录下的所有 .org 文件"
    (interactive)
    (let* ((org-dir (expand-file-name my/org-directory))
           (org-files (when (file-exists-p org-dir)
                       (directory-files org-dir t "\\.org$"))))
      (if org-files
          (ivy-read "选择 org 文件: "
                    (mapcar (lambda (file)
                             (cons (file-name-nondirectory file) file))
                           org-files)
                    :action (lambda (x) (find-file (cdr x)))
                    :caller 'my/list-org-files)
        (message "在 %s 中没有找到 org 文件" org-dir))))
  
  ;; 创建新的 org 文件的函数
  (defun my/create-org-file ()
    "在 org 目录下创建新的 org 文件"
    (interactive)
    (let* ((org-dir (expand-file-name my/org-directory))
           (filename (read-string "输入新文件名 (不需要 .org 扩展名): ")))
      (when (and filename (not (string-empty-p filename)))
        (let ((filepath (expand-file-name (concat filename ".org") org-dir)))
          (find-file filepath)
          (message "创建新的 org 文件: %s" filepath)))))
  
  ;; 启动时自动列出 org 文件的选项 (可选，取消注释下面的行来启用)
  ;; (add-hook 'emacs-startup-hook 'my/list-org-files)
  
  ;; ===== 快速打开 org 文件功能结束 =====

  ;; ===== 新增：代码块复制功能 =====
  
  ;; 复制当前代码块内容的函数
  (defun my/copy-org-src-block ()
    "复制当前代码块的内容到剪贴板"
    (interactive)
    (when (org-in-src-block-p)
      (let* ((element (org-element-at-point))
             (lang (org-element-property :language element))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element))
             (src-block (buffer-substring-no-properties begin end))
             ;; 提取代码内容（去掉 #+BEGIN_SRC 和 #+END_SRC 行）
             (lines (split-string src-block "\n"))
             (content-lines (cdr (butlast lines 1))) ; 去掉第一行和最后一行
             (content (string-join content-lines "\n")))
        (kill-new content)
        (message "已复制 %s 代码块内容到剪贴板"))
      (unless (org-in-src-block-p)
        (message "光标不在代码块内"))))
  
  ;; 复制代码块内容（仅代码，不包括语言标识）
  (defun my/copy-org-src-block-content-only ()
    "只复制代码块的代码内容，不包括语言标识和围栏"
    (interactive)
    (when (org-in-src-block-p)
      (save-excursion
        (let ((element (org-element-at-point)))
          (goto-char (org-element-property :post-affiliated element))
          (forward-line 1) ; 跳过 #+BEGIN_SRC 行
          (let ((start (point)))
            (goto-char (org-element-property :end element))
            (when (re-search-backward "^[ \t]*#\\+END_SRC" nil t)
              (let ((content (buffer-substring-no-properties start (point))))
                (kill-new content)
                (message "已复制代码内容到剪贴板"))))))
      (unless (org-in-src-block-p)
        (message "光标不在代码块内"))))
  
  ;; 复制整个代码块（包括标识符）
  (defun my/copy-org-entire-src-block ()
    "复制整个代码块，包括 #+BEGIN_SRC 和 #+END_SRC"
    (interactive)
    (when (org-in-src-block-p)
      (let* ((element (org-element-at-point))
             (begin (org-element-property :begin element))
             (end (org-element-property :end element))
             (content (buffer-substring-no-properties begin end)))
        (kill-new content)
        (message "已复制整个代码块到剪贴板"))
      (unless (org-in-src-block-p)
        (message "光标不在代码块内"))))
  
  ;; 快速选择代码块内容（用于手动复制）
  (defun my/select-org-src-block-content ()
    "选择当前代码块的内容"
    (interactive)
    (when (org-in-src-block-p)
      (let ((element (org-element-at-point)))
        (goto-char (org-element-property :post-affiliated element))
        (forward-line 1) ; 跳过 #+BEGIN_SRC 行
        (set-mark (point))
        (goto-char (org-element-property :end element))
        (when (re-search-backward "^[ \t]*#\\+END_SRC" nil t)
          ;; 移动到 #+END_SRC 行的前一行末尾
          (forward-line -1)
          (end-of-line)
          (activate-mark)
          (message "已选择代码块内容")))
      (unless (org-in-src-block-p)
        (message "光标不在代码块内"))))
  
  ;; ===== 代码块复制功能结束 =====

  ;; 配置 org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (sql . t)
     (js . t)
     (rust . t)
     (lisp . t)))
   (setq org-confirm-babel-evaluate nil)   ; 不询问是否执行代码块
   
   ;; 设置 Common Lisp 代码块的默认参数
   ;; 自定义简单的 Common Lisp 执行函数
   (defun org-babel-execute:lisp (body params)
     "Execute Common Lisp code using sbcl directly."
     (let* ((tmp-file (org-babel-temp-file "lisp-"))
            (cmd (format "sbcl --script %s" tmp-file))
            (result "")
            ;; 包装代码以确保输出结果
            (wrapped-body (format "(let ((result (progn %s)))\n  (format t \"~A~%%\" result))" body)))
       ;; 写入包装后的代码到临时文件
       (with-temp-file tmp-file
         (insert wrapped-body))
       ;; 执行并获取结果
       (with-temp-buffer
         (let ((exit-code (call-process-shell-command cmd nil t)))
           (setq result (buffer-string))
           (when (/= exit-code 0)
             (setq result (format "错误: %s" result)))))
       ;; 清理临时文件
       (delete-file tmp-file)
       ;; 返回结果，去掉多余的换行符
       (string-trim result)))
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

    ;; ===== 新增：支持依赖的 Rust 代码块功能 =====
    
    ;; 解析依赖字符串的函数
    (defun my/parse-deps-string (deps-str)
      "解析依赖字符串，返回依赖列表"
      (when (and deps-str (not (string-empty-p deps-str)))
        (let ((deps-list '())
              (current-dep "")
              (in-brace 0)
              (i 0))
          (while (< i (length deps-str))
            (let ((char (aref deps-str i)))
              (cond
               ((= char ?\") 
                (setq current-dep (concat current-dep (char-to-string char))))
               ((= char ?\{)
                (setq in-brace (1+ in-brace))
                (setq current-dep (concat current-dep (char-to-string char))))
               ((= char ?\})
                (setq in-brace (1- in-brace))
                (setq current-dep (concat current-dep (char-to-string char))))
               ((and (= char ?,) (= in-brace 0))
                (setq deps-list (append deps-list (list (string-trim current-dep))))
                (setq current-dep ""))
               (t
                (setq current-dep (concat current-dep (char-to-string char)))))
              (setq i (1+ i))))
          ;; 添加最后一个依赖
          (when (not (string-empty-p current-dep))
            (setq deps-list (append deps-list (list (string-trim current-dep)))))
          deps-list)))

    ;; 生成 Cargo.toml 内容
    (defun my/generate-cargo-toml (deps-list)
      "根据依赖列表生成 Cargo.toml 内容"
      (let ((cargo-content "[package]\nname = \"org-babel-rust\"\nversion = \"0.1.0\"\nedition = \"2021\"\n\n[dependencies]\n")
            ;; 默认依赖
            (default-deps '("serde = { version = \"1.0\", features = [\"derive\"] }"
                           "serde_json = \"1.0\"")))
        ;; 添加默认依赖
        (dolist (dep default-deps)
          (setq cargo-content (concat cargo-content dep "\n")))
        ;; 添加用户指定的依赖
        (dolist (dep deps-list)
          (setq cargo-content (concat cargo-content dep "\n")))
        cargo-content))

    ;; 创建临时项目目录
    (defun my/create-temp-rust-project (deps-list)
      "创建临时 Rust 项目，返回项目目录路径"
      (let* ((temp-dir (make-temp-file "org-rust-" t))
             (src-dir (expand-file-name "src" temp-dir))
             (cargo-toml (expand-file-name "Cargo.toml" temp-dir))
             (main-rs (expand-file-name "main.rs" src-dir)))
        ;; 创建 src 目录
        (make-directory src-dir t)
        ;; 生成 Cargo.toml
        (write-region (my/generate-cargo-toml deps-list) nil cargo-toml)
        ;; 创建空的 main.rs
        (write-region "" nil main-rs)
        temp-dir))

    ;; 清理临时项目
    (defun my/cleanup-temp-project (temp-dir)
      "清理临时项目目录"
      (when (and temp-dir (file-exists-p temp-dir))
        (delete-directory temp-dir t)))

    ;; 自定义的 Rust 代码块执行函数
    (defun org-babel-execute:rust (body params)
      "执行 Rust 代码块，支持依赖管理"
      (let* ((deps-str (cdr (assq :deps params)))
             (deps-list (my/parse-deps-string deps-str))
             (temp-dir (my/create-temp-rust-project deps-list))
             (main-rs (expand-file-name "src/main.rs" temp-dir))
             (output nil)
             (error-output nil))
        (unwind-protect
            (progn
              ;; 写入代码到 main.rs
              (write-region body nil main-rs)
              
              ;; 执行 cargo run --quiet 来抑制编译信息
              (let ((default-directory temp-dir))
                (with-temp-buffer
                  (let ((exit-code (call-process "cargo" nil t nil "run" "--quiet")))
                    (setq output (buffer-string))
                    (when (/= exit-code 0)
                      (setq error-output output)
                      (setq output nil)))))
              
              ;; 返回结果
              (if error-output
                  (format "错误:\n%s" error-output)
                output))
          ;; 清理临时文件
          (my/cleanup-temp-project temp-dir))))

    ;; ===== 新增功能结束 =====

  ;; 设置省略号样式和标题对齐
  (setq org-ellipsis " ⤵ ")
  (set-face-attribute 'org-ellipsis nil :foreground "#E6DC88")
  (set-face-attribute 'org-level-1 nil :height 1.2 :weight 'normal)
  (set-face-attribute 'org-level-2 nil :height 1.1 :weight 'normal)
  (set-face-attribute 'org-level-3 nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-4 nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-5 nil :height 1.0 :weight 'normal)
  (set-face-attribute 'org-level-6 nil :height 1.0 :weight 'normal)
  ;; 你可以根据需要继续设置 org-level-5 等
  :bind
  (("C-c a" . org-agenda)                 ; 打开议程视图
   ("C-c c" . org-capture)                ; 快速捕获
   ("C-c l" . org-store-link)             ; 存储链接
   ("C-c C-," . org-insert-structure-template)  ; 插入代码块
   ("C-c C-'" . org-edit-special)         ; 编辑代码块
   ("C-c C-c" . org-babel-execute-src-block) ; 执行代码块
   ("C-c o" . my/list-org-files)          ; 列出 org 文件
   ("C-c O" . my/open-org-file)           ; 打开 org 目录
   ("C-c n" . my/create-org-file)         ; 创建新的 org 文件
   ("C-c y" . my/copy-org-src-block-content-only) ; 复制代码块内容
   ("C-c Y" . my/copy-org-entire-src-block) ; 复制整个代码块
   ("C-c s" . my/select-org-src-block-content))) ; 选择代码块内容

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
    (kbd "TAB") 'org-cycle)
  ;; 添加代码块复制的 Evil 快捷键
  (evil-define-key 'normal evil-org-mode-map
    "yc" 'my/copy-org-src-block-content-only  ; 复制代码块内容
    "yC" 'my/copy-org-entire-src-block        ; 复制整个代码块
    "vc" 'my/select-org-src-block-content))   ; 选择代码块内容 (visual mode)

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
   '(bing-dict company counsel-projectile default-text-scale doom-themes
	       emojify evil-collection evil-embrace evil-escape
	       evil-org evil-textobj-entire ivy-rich lsp-ui rg rustic
	       sdcv slime transpose-frame treemacs-all-the-icons
	       treemacs-evil treemacs-projectile undo-tree valign
	       vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
