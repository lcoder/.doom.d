;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Source Code Pro ;; Menlo ;; Source Code Pro ;; SF Mono ;; Sarasa Term SC Nerd
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 15))

;; 通用小优化：禁用双向文本重排，降低重绘开销（不编辑 RTL 语言时安全）
(setq-default bidi-paragraph-direction 'left-to-right)

;; org 模式下启用 bidi 经常会导致崩溃，因此禁用
;; 进一步规避 bidi 相关崩溃（如 bidi_pop_it / posn_at_point）
(setq-default bidi-display-reordering nil)
(setq-default bidi-inhibit-bpa t)

;; 避免 Emacs 30 上 smartparens 的内部变量未初始化导致
;; (void-variable smartparens-mode--suppress-set-explicitly) 报错：
;; 提前加载 smartparens，确保其 minor-mode 正确定义
(use-package! smartparens
  :demand t)

;; smartSelect: Alt+o 扩大选区, Alt+p 缩小选区（全局）
(use-package! expreg
  :commands (expreg-expand expreg-contract)
  :init
  (map! :nvi "M-o" #'expreg-expand
        :nvi "M-p" #'expreg-contract)
  ;; 仅在文本类模式启用句子级扩选（官方建议）
  (after! expreg
    (defun my/enable-expreg-sentence-in-text-mode ()
      (make-local-variable 'expreg-functions)
      (add-to-list 'expreg-functions #'expreg--sentence))
    (add-hook 'text-mode-hook #'my/enable-expreg-sentence-in-text-mode))
  ;; 在 rustic-mode 中确保存在 rust 的 tree-sitter 解析器(为了配合expreg扩展使用，利用treesit的语法树)
  (after! rustic
    (defun my/rustic-ensure-treesit-parser ()
      (when (and (treesit-available-p)
                 (treesit-language-available-p 'rust)
                 (null (treesit-parser-list)))
        (ignore-errors (treesit-parser-create 'rust))))
    (add-hook 'rustic-mode-hook #'my/rustic-ensure-treesit-parser)
    ;; 设置 rustic-mode 的缩进为2个空格
    (setq rustic-indent-offset 2)))

;; --- start of 展示当前key的日志 ---
(defvar dw/command-window-frame nil)
(defun dw/toggle-command-window ()
  (interactive)
  (require 'posframe)
  (require 'command-log-mode)
  (if dw/command-window-frame
      (progn
        (posframe-delete-frame clm/command-log-buffer)
        (setq dw/command-window-frame nil))
    (progn
      (global-command-log-mode t)
      (setq clm/command-log-buffer (get-buffer-create " *command-log*"))
      (with-current-buffer clm/command-log-buffer
        (setq-local face-remapping-alist
                    (cons '(default :height 120)
                          face-remapping-alist)))
      (let* ((cols 35)
             (lines 4)
             (border 2)
             (cw (frame-char-width))
             (ch (frame-char-height))
             (posw (+ (* cols cw) (* 2 border)))
             (x (- (frame-pixel-width) 10 posw))
             (y 10))
        (setq dw/command-window-frame
              (posframe-show
               clm/command-log-buffer
               :position (cons x y)
               :width cols
               :height lines
               :min-width cols
               :min-height lines
               :internal-border-width border
               :internal-border-color "#c792ea"))))))
;; Bind a convenient key (SPC t k) to toggle the command window
(map! :leader
      :desc "Toggle command log window"
      "t k" #'dw/toggle-command-window)
;; --- end of 展示当前key的日志 ---

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "～/Library/Mobile Documents/com~apple~CloudDocs/org/")
;; org-mode 崩溃/报错监控日志（便于事后分析）
;; 访问~/.config/doom/org-crash.log 查看报错日志
(defvar my/org-crash-log-file (expand-file-name "org-crash.log" doom-user-dir))
(defvar my/org--command-error-fn command-error-function)
(defvar my/org--ignored-errors
  '(quit beginning-of-line end-of-line beginning-of-buffer end-of-buffer))
(defvar my/org-crash-debug-on-error nil)

(defun my/org--log (fmt &rest args)
  (with-temp-buffer
    (insert (format-time-string "[%Y-%m-%d %H:%M:%S] "))
    (insert (apply #'format fmt args))
    (insert "\n")
    (append-to-file (point-min) (point-max) my/org-crash-log-file)))

(defun my/org--ignorable-error-p (err)
  (memq (car-safe err) my/org--ignored-errors))

(defun my/org--key-desc ()
  (let ((keys (this-command-keys-vector)))
    (when (and keys (> (length keys) 0))
      (key-description keys))))

(defun my/org--messages-tail (lines)
  (when (get-buffer "*Messages*")
    (with-current-buffer "*Messages*"
      (save-excursion
        (goto-char (point-max))
        (forward-line (- lines))
        (buffer-substring-no-properties (point) (point-max))))))

(defun my/org--log-error (err context caller)
  (my/org--log "error: %s" (error-message-string err))
  (my/org--log "command: %s caller: %s context: %s keys: %s"
               (or this-command last-command "N/A")
               (or caller "N/A")
               (or context "N/A")
               (or (my/org--key-desc) "N/A"))
  (my/org--log "major-mode: %s evil-state: %s"
               major-mode
               (if (boundp 'evil-state) evil-state "N/A"))
  (my/org--log "buffer: %s file: %s point: %s"
               (buffer-name)
               (or (buffer-file-name) "N/A")
               (point))
  (my/org--log "emacs: %s system: %s" emacs-version system-type)
  (my/org--log "backtrace:\n%s"
               (with-temp-buffer
                 (backtrace)
                 (buffer-string)))
  (let ((msgs (my/org--messages-tail 200)))
    (when msgs
      (my/org--log "messages:\n%s" msgs)))
  (my/org--log "----"))

(defun my/org-command-error-logger (data context caller)
  (when (and (derived-mode-p 'org-mode)
             (not (my/org--ignorable-error-p data)))
    (condition-case nil
        (my/org--log-error data context caller)
      (error nil)))
  (when my/org--command-error-fn
    (funcall my/org--command-error-fn data context caller)))

(setq command-error-function #'my/org-command-error-logger)
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local debug-on-error my/org-crash-debug-on-error)))
;; org-modern SF Mono
;; ellipsis https://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(after! org
  (setq org-ellipsis " ⤵")
  ;; 设置代码块默认缩进为2个空格
  (setq org-edit-src-content-indentation 2)
  (custom-set-faces
   '(org-ellipsis ((t (:foreground "#E6DC88")))))
  ;; org-mode 使用 Sarasa Term SC Nerd，其他模式保持默认 FiraCode Nerd Font
  (add-hook 'org-mode-hook
            (lambda ()
              (face-remap-add-relative 'default :family "Sarasa Term SC Nerd")
              (set-fontset-font t 'han (font-spec :family "Sarasa Term SC Nerd") nil 'prepend))))

;; 更简洁：基于主题设置，仅改字重，继承 outline-N 以保留颜色等样式
(after! org
  (custom-theme-set-faces! 'user
    '(org-document-title :weight normal)
    '(org-level-1 :inherit outline-1 :weight normal)
    '(org-level-2 :inherit outline-2 :weight normal)
    '(org-level-3 :inherit outline-3 :weight normal)
    '(org-level-4 :inherit outline-4 :weight normal)
    '(org-level-5 :inherit outline-5 :weight normal)
    '(org-level-6 :inherit outline-6 :weight normal)
    '(org-level-7 :inherit outline-7 :weight normal)
    '(org-level-8 :inherit outline-8 :weight normal)))

;; 改用这个 jk -> esc
;; 参考：https://emacs-china.org/t/evil-mode-insert-mode-emacs-easy-c-n-c-p/22512/11
(after! evil-escape
  (setq-default evil-escape-delay 0.3)
  (setq evil-escape-excluded-major-modes '(dired-mode))
  (delete 'visual evil-escape-excluded-states)
  (setq-default evil-escape-key-sequence "jk"))

;; treesit
(use-package! treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode -1)
  (add-hook 'prog-mode-hook #'treesit-auto-mode))

;; 针对前端项目 自动开启lsp
(dolist (hook '(typescript-ts-mode-local-vars-hook
                tsx-ts-mode-local-vars-hook
                js-ts-mode-local-vars-hook
                json-ts-mode-local-vars-hook
                ;; 兼容未进 ts-mode 时的回退模式
                typescript-mode-local-vars-hook
                js-mode-local-vars-hook
                json-mode-local-vars-hook
                web-mode-local-vars-hook))
  (add-hook hook #'lsp!))

;; 添加项目搜索目录
(after! projectile
  (when (file-exists-p "~/.config/doom/local.el")
    (load! "local"))
  (unless projectile-project-search-path
    (setq projectile-project-search-path '("~/workspace/")))) ;; 默认项目搜索路径

;; 自动跟踪当前buffer
(after! treemacs
  (treemacs-follow-mode 1)
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (when (bound-and-true-p treemacs-project-follow-mode)
                ;; 关闭treemacs自动切换项目
                (treemacs-project-follow-mode -1)))))

;; 修改下划线为单词字符
(modify-syntax-entry ?_ "w")

;; vterm 字符 自动贴左
(after! vterm
  (defun my/vterm-stick-left ()
    (setq-local auto-hscroll-mode nil
                hscroll-margin 0
                truncate-lines t)
    (set-window-hscroll (selected-window) 0))
  (add-hook 'vterm-mode-hook #'my/vterm-stick-left))
;; vterm放大/缩小时也强制贴左
(defun my/vterm-reset-hscroll (&rest _)
  (when (derived-mode-p 'vterm-mode)
    (set-window-hscroll (selected-window) 0)))
(advice-add 'text-scale-adjust :after #'my/vterm-reset-hscroll)

;; pyim（从零：先保证基础词库可用，再叠加清华词库）
;;
;; 用法：
;; - `C-\\` 切换输入法（Emacs 默认键位）
;; - 切到 pyim 后，输入 shuzu 应该能得到「数组」
(after! pyim
  (setq default-input-method "pyim"
        pyim-default-scheme 'quanpin
        ;; GUI 优先用 posframe；不行就回退到 minibuffer
        pyim-page-tooltip '(posframe minibuffer)
        pyim-page-length 8
        ;; 性能/隐私：默认关云输入；关 buffer 搜词（容易卡）
        pyim-cloudim nil
        pyim-candidates-search-buffer-p nil)

  ;; 标点：永远半角（包括中文注释）
  (setq-default pyim-punctuation-translate-p '(auto))
  ;; 模糊拼音
  (setq pyim-pinyin-fuzzy-alist
        '(("en" "eng")
          ("in" "ing")
          ("an" "ang")
          ("ian" "iang")
          ("uan" "uang")
          ("c" "ch")
          ("s" "sh")
          ("z" "zh")
          ("l" "n")))
  ;; 程序员友好：prog-mode 下默认英文，仅在注释/字符串里中文
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-program-mode
                  pyim-probe-isearch-mode))
  ;; 基础词库（优先保证常用词可用）
  (when (require 'pyim-basedict nil t)
    (pyim-basedict-enable))
  ;; 清华词库（可选叠加）
  (when (require 'pyim-tsinghua-dict nil t)
    (require 'pyim-dict nil t)
    (pyim-tsinghua-dict-enable))
  ;; 不切输入法也能临时纯英文输入
  (define-key pyim-mode-map (kbd "C-.") #'pyim-toggle-input-ascii))

;; org 写中文：允许中文标点（不强制半角）
(add-hook 'org-mode-hook
          (lambda ()
            ;; org 里更偏中文输入（isearch 时仍强制英文）
            (setq-local pyim-english-input-switch-functions
                        '(pyim-probe-isearch-mode))
            ;; 取消“行首/标点后强制半角”的探针
            (setq-local pyim-punctuation-half-width-functions nil)))

;; org roam https://www.skfwe.cn/p/org-roam-%E4%BD%BF%E7%94%A8/
;; org roam pr: https://github.com/doomemacs/doomemacs/pull/5271/files
(use-package! org-roam
  :config
  (setq org-roam-directory (expand-file-name "roam")))

;; rust下的格式化
(after! apheleia
  (set-formatter! 'rustfmt
    '("rustfmt" "--edition" "2024")
    :modes '(rust-mode rustic-mode rust-ts-mode)))

;; 关闭treemacs自动追踪（已在前面 after! treemacs 中统一处理）
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
