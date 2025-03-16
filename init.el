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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 (set-face-attribute 'default nil :font "FiraMono Nerd Font"))
