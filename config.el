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
    (add-hook 'rustic-mode-hook #'my/rustic-ensure-treesit-parser)))

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
;; org-modern SF Mono
;; ellipsis https://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(after! org
  (setq org-ellipsis " ⤵")
  (custom-set-faces
   '(org-ellipsis ((t (:foreground "#E6DC88")))))
  ;; org-mode 使用 Sarasa Term SC Nerd，其他模式保持默认 FiraCode Nerd Font
  (add-hook 'org-mode-hook
            (lambda ()
              (face-remap-add-relative 'default :family "Sarasa Term SC Nerd"))))

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
  (global-treesit-auto-mode))

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

;; 默认同意监视大型项目（抑制“watch all files”提示）
;; Watching all the files in /foo would require adding watches to 3792 directories, so watching the repo may slow Emacs down. Do you want to watch all files in /foo? (y or n) n
(after! lsp-mode
  (dolist (dir '("[/\\]dist\'"
                 "[/\\]build\'"
                 "[/\\]target\'"))
    (add-to-list 'lsp-file-watch-ignored-directories dir))
  (setq lsp-warn-project-dir-watchers-too-many nil
        lsp-file-watch-threshold 6000))

;; 添加项目搜索目录
(after! projectile
  (when (file-exists-p "~/.config/doom/local.el")
    (load! "local"))
  (unless projectile-project-search-path
    (setq projectile-project-search-path '("~/workspace/")))) ;; 默认项目搜索路径

;; 自动跟踪当前buffer
(after! treemacs
  (treemacs-project-follow-mode 1))

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

;; 启用 pyim-greatdict 词库
(use-package! pyim-greatdict
  :config
  (pyim-greatdict-enable))

;; pyim
(after! pyim
  (setq default-input-method "pyim")
  (setq pyim-page-tooltip 'posframe)
  (setq pyim-page-length 8)
  (setq pyim-cloudim 'baidu)
  (setq pyim-default-scheme 'quanpin)
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
  ;; 使用半角标点（自动）
  (setq-default pyim-punctuation-translate-p '(auto))
  (setq-default pyim-punctuation-half-width-functions
                ;; 行首强制输入半角标点
                '(pyim-probe-punctuation-line-beginning
                  ;; 半角标点后强制输入半角标点
                  pyim-probe-punctuation-after-punctuation))
  ;; 临时切换中英文
  (define-key pyim-mode-map (kbd "C-.") 'pyim-toggle-input-ascii))
;; 让插入态也能识别 C-.（即使某些情况下 pyim-mode-map 没挂上）
(after! evil
  (define-key evil-insert-state-map (kbd "C-.") #'pyim-toggle-input-ascii))

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
