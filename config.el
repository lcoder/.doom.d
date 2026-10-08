;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+ui")
(load! "+key-observer")

;; Set the notes location without loading Org or its database integration.
(setq org-roam-directory (expand-file-name "roam" (or org-directory "~/org")))

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
(setq doom-theme 'doom-moonlight)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; 补全弹窗默认选中第一项，Enter 接受候选而不换行。
(after! corfu
  (setq corfu-preselect 'first
        +corfu-want-ret-to-confirm t))

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

;; 已有的空括号也能用回车展开；缩进由当前语言模式决定。
(defun my/empty-code-pair-bounds ()
  "Return the bounds inside an empty code pair on the current line."
  (when (and (derived-mode-p 'prog-mode)
             (bound-and-true-p smartparens-mode)
             (not (nth 8 (syntax-ppss))))
    (save-excursion
      (let* ((start (progn (skip-chars-backward " \t") (point)))
             (open (char-before))
             (end (progn (skip-chars-forward " \t") (point))))
        (when (and (memq open '(?\( ?\[ ?\{))
                   (eq (char-after) (matching-paren open)))
          (cons start end))))))

(defun my/expand-empty-code-pair-on-newline-a (fn &rest args)
  "Expand an empty code pair when FN inserts a single interactive newline."
  (let ((bounds (and (memq this-command
                           '(newline newline-and-indent
                             electric-newline-and-maybe-indent))
                     (memq last-command-event '(10 13 return))
                     (or (null (car args)) (equal (car args) 1))
                     (not (use-region-p))
                     (my/empty-code-pair-bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds)))
    (prog1 (apply fn args)
      (when bounds
        (save-excursion
          (newline)
          (indent-according-to-mode))
        (indent-according-to-mode)))))

(after! smartparens
  ;; 替换只在刚插入括号后生效的 RET 延迟处理，避免重复换行。
  (dolist (pair '("(" "[" "{"))
    (sp-local-pair 'prog-mode pair nil
                   :post-handlers '(:rem ("||\n[i]" "RET"))))
  (advice-add #'newline :around #'my/expand-empty-code-pair-on-newline-a))

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
    (add-hook 'text-mode-hook #'my/enable-expreg-sentence-in-text-mode)))

;; Bind a convenient key (SPC t k) to toggle the command window
(map! :leader
      :desc "Toggle command log window"
      "t k" #'dw/toggle-command-window)
;; --- end of 展示当前key的日志 ---

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

(defun my/browse-notes ()
  "Open `org-directory' directly."
  (interactive)
  (find-file (my/org-directory)))

(defun my/find-in-notes ()
  "Find a file under `org-directory'."
  (interactive)
  (let ((default-directory (my/org-directory)))
    (+vertico/consult-fd-or-find default-directory)))

(map! :leader
      :desc "Find file in notes" "n f" #'my/find-in-notes
      :desc "Browse notes"       "n F" #'my/browse-notes)
;; org-modern SF Mono
;; ellipsis https://endlessparentheses.com/changing-the-org-mode-ellipsis.html
(after! org
  (setq org-ellipsis " ⤵"
        org-edit-src-content-indentation 2)
  (custom-set-faces
   '(org-ellipsis ((t (:foreground "#E6DC88"))))))

(use-package! valign
  :hook (org-mode . valign-mode))

;; org-babel: 允许执行 dart/flutter 代码块
(after! org
  (require 'ob-dart)
  (add-to-list 'org-babel-load-languages '(dart . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages)
  ;; dart 输出默认按 raw 插入，避免 Org 对逗号做转义
  (setq org-babel-default-header-args:dart
        '((:results . "output raw"))))

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

;; 中文 surround：左右标点均可触发，z 是中文圆括号的英文别名。
;; 配对：（）【】《》〈〉「」『』“”‘’，新增时不附加空格。
;; 示例：ysiwz 添加（），dsz 删除（），csz) 换成无空格的英文括号。
(after! evil-surround
  (let ((pairs (copy-tree (default-value 'evil-surround-pairs-alist))))
    (dolist (pair '(("（" . "）") ("【" . "】")
                    ("《" . "》") ("〈" . "〉")
                    ("「" . "」") ("『" . "』")
                    ("“" . "”") ("‘" . "’")))
      (dolist (delimiter (list (car pair) (cdr pair)))
        (setf (alist-get (string-to-char delimiter) pairs) pair)))
    (setf (alist-get ?z pairs) '("（" . "）"))
    (setq-default evil-surround-pairs-alist pairs))
  ;; Doom 的 evil-embrace 会接管其他字符，需将这些触发键交回 surround。
  (after! evil-embrace
    (let ((keys (copy-sequence (default-value 'evil-embrace-evil-surround-keys))))
      (dolist (key (string-to-list "（）【】《》〈〉「」『』“”‘’z"))
        (cl-pushnew key keys))
      (setq-default evil-embrace-evil-surround-keys keys))))

;; jk 仅退出插入和替换状态，保留导航及特殊窗口的原有按键行为。
(after! evil-escape
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.2
                evil-escape-unordered-key-sequence nil)

  (defun my/evil-escape--outside-editing-p ()
    "Return non-nil outside Evil insert and replace states."
    (not (memq evil-state '(insert replace))))

  (add-hook 'evil-escape-inhibit-functions #'my/evil-escape--outside-editing-p)
  ;; 补回旧配置覆盖的默认保护，同时保留其他配置追加的排除项。
  (dolist (mode '(neotree-mode treemacs-mode vterm-mode ghostel-mode dired-mode))
    (add-to-list 'evil-escape-excluded-major-modes mode))
  (add-to-list 'evil-escape-excluded-states 'visual))

;; 文件路径不做拼音/正则扩展：长路径可能生成过大的正则，导致文件选择失败。
;; 独立样式保留无序多关键词匹配，不影响其他补全类别或中文输入。
(after! orderless
  (defvar my/orderless-file-compat-warned nil)
  (let ((styles '(partial-completion basic)))
    (if (fboundp 'orderless-define-completion-style)
        (progn
          ;; 延迟宏展开，使缺少该宏的旧版 Orderless 也能加载此配置。
          (eval '(orderless-define-completion-style my/orderless-file
                   "Literal multi-component completion for file paths."
                   (orderless-matching-styles '(orderless-literal))
                   (orderless-style-dispatchers nil)))
          (setq styles '(my/orderless-file partial-completion basic)))
      ;; 旧版保留基本路径补全；每个 Emacs 会话只提示一次。
      (unless my/orderless-file-compat-warned
        (setq my/orderless-file-compat-warned t)
        (display-warning 'my/orderless-file
                         "Orderless 较旧：文件补全已回退为基本路径匹配，不支持无序多关键词。"
                         :warning)))
    (dolist (category '(file project-file))
      (setf (alist-get 'styles (alist-get category completion-category-overrides))
            styles)))
  ;; Emacs 会把全局 styles 追加到类别 styles 后；无匹配时仍会落入拼音规则。
  ;; 仅在文件补全调用内禁止该回退，其他类别继续使用原有全局 styles。
  (defun my/file-completion-styles-a (fn string table pred point &optional metadata)
    (let* ((metadata (or metadata (completion-metadata string table pred)))
           (completion-styles
            (unless (and (memq (completion-metadata-get metadata 'category)
                               '(file project-file))
                         ;; Consult 在外层拆分异步查询，内层再应用文件匹配规则。
                         (not (equal completion-styles '(consult--split))))
              completion-styles)))
      (funcall fn string table pred point metadata)))
  (dolist (fn '(completion-try-completion completion-all-completions))
    (advice-add fn :around #'my/file-completion-styles-a)))

;; 文件位置引用：path:line[:column[:tag]]，各文件打开入口共用同一套规则。
(defun my/file-location--parse (input)
  "Split INPUT into a path, line and optional column without accessing files.
Keep number strings for validation after checking for a literal file name."
  ;; 普通路径直接返回，避免每次打开文件都构造并匹配完整正则。
  (when (and (stringp input) (string-search ":" input))
    (save-match-data
      (let ((prefix "\\`\\([^\n\r]+\\):\\([-+]?[0-9]+\\)"))
        (cond
         ((or (string-match
               (concat prefix ":\\([^:\n\r]*\\):[^:0-9+\n\r-][^:\n\r]*\\'") input)
              (string-match (concat prefix ":\\([^:\n\r]*\\)\\'") input))
          (list (match-string 1 input) (match-string 2 input) (match-string 3 input)))
         ((string-match (concat prefix "\\'") input)
          (list (match-string 1 input) (match-string 2 input) nil)))))))

(defun my/file-location--resolve (input &optional directory)
  "Resolve a local INPUT reference relative to DIRECTORY to (FILE LINE COLUMN).
Return nil for ordinary or remote names and existing literal file names.
Signal a user error for invalid positions or missing reference targets."
  (when-let* ((parts (my/file-location--parse input))
              ((not (or (file-name-quoted-p input)
                        (string-match-p "\\`[[:alpha:]][[:alnum:]+.-]*://" input)
                        (file-remote-p input))))
              (expanded (expand-file-name input directory))
              ((not (or (file-remote-p expanded) (file-exists-p expanded)))))
    (let ((file (expand-file-name (car parts) directory))
          (line (nth 1 parts))
          (column (or (nth 2 parts) "1")))
      (unless (and (string-match-p "\\`[0-9]+\\'" line)
                   (string-match-p "\\`[0-9]+\\'" column)
                   (> (string-to-number line) 0)
                   (> (string-to-number column) 0))
        (user-error "行号和列号必须是从 1 开始的整数"))
      (unless (file-regular-p file)
        (user-error "文件不存在或不是普通文件：%s" file))
      (list file (string-to-number line) (string-to-number column)))))

(defun my/file-location--open-a (fn filename &rest args)
  "Open FILENAME through FN with ARGS, then visit its requested position."
  (if-let* ((location (my/file-location--resolve filename)))
      ;; 已解析的引用指向具体文件，不能把 Next.js 的 [slug] 当成通配符。
      (let ((find-file-wildcards nil))
        (prog1 (apply fn (car location) args)
          ;; 明确的位置引用优先于历史光标位置，并使用文件的实际行号。
          (widen)
          (goto-char (point-min))
          (forward-line (min (1- (nth 1 location)) (buffer-size)))
          (forward-char (min (1- (nth 2 location))
                             (- (line-end-position) (point))))))
    (apply fn filename args)))

(dolist (fn '(find-file find-file-other-window find-file-other-frame
              find-file-read-only find-file-read-only-other-window
              find-file-read-only-other-frame find-file-existing
              find-file-literally))
  (advice-add fn :around #'my/file-location--open-a))

(defvar-local my/file-location--directory nil
  "Base directory for position references in this file-opening minibuffer.
Nil disables the input adapter, including in unrelated or nested prompts.")

(defun my/file-location--setup-h ()
  "Enable position input in the current file-opening minibuffer."
  (when (and (not (file-remote-p default-directory))
             (memq (completion-metadata-get
                    (completion-metadata (minibuffer-contents-no-properties)
                                         minibuffer-completion-table
                                         minibuffer-completion-predicate)
                    'category)
                   '(file project-file)))
    (setq-local my/file-location--directory default-directory)))

(defun my/file-location--read-a (fn &rest args)
  "Enable position input for the file reader FN called with ARGS."
  (minibuffer-with-setup-hook (:append #'my/file-location--setup-h)
    (apply fn args)))

(advice-add #'find-file-read-args :around #'my/file-location--read-a)

(after! project
  ;; 也覆盖 SPC f F 在非项目目录中使用的原生 project.el 查找。
  (advice-add #'project-find-file-in :around #'my/file-location--read-a))

(defvar my/file-location--projectile-context nil
  "Non-nil only while a supported Projectile file opener is running.
The value `root' means paths are relative to the project root; `directory'
means they are relative to the reader's current directory.")

(defun my/file-location--projectile-root-a (fn &rest args)
  "Run the Projectile file opener FN with ARGS relative to its project root."
  (let ((my/file-location--projectile-context 'root))
    (apply fn args)))

(defun my/file-location--projectile-directory-a (fn &rest args)
  "Run the Projectile file opener FN with ARGS relative to its directory."
  (let ((my/file-location--projectile-context 'directory))
    (apply fn args)))

(defun my/file-location--projectile-read-a (fn prompt choices &rest args)
  "Enable position references in Projectile file-opening readers only.
Pass PROMPT, CHOICES and ARGS through to FN unchanged."
  (if (and my/file-location--projectile-context
           (eq (plist-get args :caller) 'projectile-read-file))
      (let ((directory (if (eq my/file-location--projectile-context 'root)
                           (projectile-project-root)
                         default-directory)))
        (minibuffer-with-setup-hook
            (:append (lambda ()
                       (my/file-location--setup-h)
                       (when my/file-location--directory
                         (setq-local my/file-location--directory directory))))
          (apply fn prompt choices args)))
    (apply fn prompt choices args)))

(after! projectile
  (dolist (fn '(projectile--find-file projectile--find-file-dwim
                projectile-find-file-all))
    (when (fboundp fn)
      (advice-add fn :around #'my/file-location--projectile-root-a)))
  (dolist (fn '(projectile-find-file-in-directory
                projectile-find-file-in-known-projects))
    (when (fboundp fn)
      (advice-add fn :around #'my/file-location--projectile-directory-a)))
  (advice-add #'projectile-completing-read :around #'my/file-location--projectile-read-a))

(after! consult
  (dolist (fn '(consult-recent-file consult--find))
    (when (fboundp fn)
      (advice-add fn :around #'my/file-location--read-a))))

(defun my/file-location--minibuffer-input ()
  "Return a file reference from the current input, respecting Consult splitting.
Return nil without file checks when no numeric position marker is present.
Leave a Consult query with a separate nonempty filter to normal completion."
  (let ((input (minibuffer-contents-no-properties)))
    (when (string-match-p ":[-+]?[0-9]" input)
      (cond
       (minibuffer-completing-file-name
        (substitute-in-file-name input))
       ((and (equal completion-styles '(consult--split))
             (boundp 'consult-async-split-styles-alist)
             ;; 直接输入的绝对或显式相对路径不是 Consult 的分隔符。
             (not (string-match-p "\\`\\(?:/\\|~/\\|\\.\\.?/\\)" input))
             (not (file-exists-p (expand-file-name input my/file-location--directory))))
        (let* ((style (alist-get (or consult-async-split-style 'none)
                                 consult-async-split-styles-alist))
               (splitter (plist-get style :function))
               (parts (and splitter (funcall splitter input style))))
          (when (and parts (= (cadr parts) (length input)))
            (car parts))))
       (t input)))))

(defun my/file-location--vertico-exit-a (fn &rest args)
  "Accept a position reference before FN chooses a completion candidate.
Only enabled file-opening prompts may bypass candidate membership checks."
  (let ((location (and my/file-location--directory
                       (my/file-location--resolve
                        (my/file-location--minibuffer-input)
                        my/file-location--directory))))
    (if (not location)
        (apply fn args)
      (let ((input (format "%s:%d:%d" (nth 0 location)
                           (nth 1 location) (nth 2 location))))
        (delete-minibuffer-contents)
        (insert (if minibuffer-completing-file-name
                    (minibuffer-maybe-quote-filename input)
                  input)))
      (exit-minibuffer))))

(after! vertico
  (advice-add #'vertico-exit :around #'my/file-location--vertico-exit-a))

(after! yasnippet
  (make-directory (expand-file-name "snippets/" doom-user-dir) t))

;; 使用各语言自己的项目发现机制；优先采用项目安装的工具版本。
(after! lsp-dart
  (setq lsp-dart-project-root-discovery-strategies '(closest-pubspec lsp-root)))

(after! lsp-javascript
  (setq lsp-clients-typescript-prefer-use-project-ts-server t))

;; 自动跟踪当前buffer
(after! treemacs
  (treemacs-follow-mode 1)
  (add-hook 'treemacs-mode-hook
            (lambda ()
              (when (bound-and-true-p treemacs-project-follow-mode)
                ;; 关闭treemacs自动切换项目
                (treemacs-project-follow-mode -1)))))

;; 修改下划线为单词字符
(defun my/treat-underscore-as-word ()
  (modify-syntax-entry ?_ "w"))

(add-hook 'prog-mode-hook #'my/treat-underscore-as-word)
(add-hook 'text-mode-hook #'my/treat-underscore-as-word)

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
        ;; 固定在 minibuffer 显示候选词，规避 posframe 定位触发的崩溃
        pyim-page-tooltip 'minibuffer
        pyim-page-length 8
        ;; 性能/隐私：默认关云输入；关 buffer 搜词（容易卡）
        pyim-cloudim nil
        pyim-candidates-search-buffer-p nil)

  ;; 标点随输入状态自动切换：中文全角、英文半角。
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
