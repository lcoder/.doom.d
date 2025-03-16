;; 确保包管理器已配置 MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(package-refresh-contents)

;; 下载 Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; 开启 Evil
(require 'evil)
(evil-mode 1)

;; 终端
(use-package vterm
  :ensure t)

;; 主题
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

;; 配置内置的 lsp-mode
(use-package lsp-mode
  :hook (rustic-mode . lsp)  ;; 在 rustic-mode 中自动启用 LSP
  :config
  ;; 可选：调整 lsp-mode 的行为，例如提高性能
  (setq lsp-idle-delay 0.5))

;; 安装 rustic（用于 Rust 开发）
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t))

;; 安装 lsp-ui（增强 LSP 的 UI，如悬浮文档、错误提示等）
(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-ui-sideline-enable t
        lsp-ui-doc-enable t))

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
 )
