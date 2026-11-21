;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(load! "+ui")
(load! "+org-src")

(setq doom-theme 'doom-moonlight
      display-line-numbers-type 'relative
      org-roam-directory (expand-file-name "roam" org-directory))

;; 补全弹窗默认选中第一项，Enter 接受候选而不换行。
(after! corfu
  (setq corfu-preselect 'first
        +corfu-want-ret-to-confirm t))

;; 本地项目每次读取文件列表时重新枚举，及时发现外部工具的文件变动。
(after! projectile
  (defun my/projectile-project-files--fresh-local-a (fn project-root)
    "Call FN for PROJECT-ROOT with fresh local files and unchanged remote caching."
    (let ((projectile-enable-caching
           (and (file-remote-p project-root) projectile-enable-caching)))
      (funcall fn project-root)))

  (advice-add #'projectile-project-files :around
              #'my/projectile-project-files--fresh-local-a))

;; 固定段落从左到右，保留与 Doom 字体初始化一致的双向重排设置。
(setq-default bidi-paragraph-direction 'left-to-right
              bidi-display-reordering t
              bidi-inhibit-bpa t)

(defun my/enable-expreg-sentence-in-text-mode ()
  "Include the current sentence when expanding a text selection."
  (require 'expreg)
  (setq-local expreg-functions
              (cons #'expreg--sentence (remq #'expreg--sentence expreg-functions))))

(use-package! expreg
  :commands (expreg-expand expreg-contract)
  :init
  (map! :map text-mode-map
        :nvi "M-o" #'expreg-expand
        :nvi "M-p" #'expreg-contract)
  (add-hook 'text-mode-hook #'my/enable-expreg-sentence-in-text-mode)
  :config
  (defun my/expreg-preserve-manual-region-a (fn &rest args)
    "Make FN expand a new character region without dropping selected text."
    (let ((bounds (and (use-region-p)
                       (not (bound-and-true-p rectangle-mark-mode))
                       (not (and (eq (bound-and-true-p evil-state) 'visual)
                                 (memq evil-visual-selection '(line screen-line block))))
                       (cons (region-beginning) (region-end)))))
      (if (or (null bounds)
              (= (car bounds) (cdr bounds))
              (equal bounds (cdar expreg--prev-regions)))
          (apply fn args)
        (let* ((functions expreg-functions)
               ;; Keep even a single selected bracket in the contraction history.
               (expreg--validation-white-list
                (cons 'my/expreg-manual-region expreg--validation-white-list))
               (expreg-functions
                (list
                 (lambda ()
                   (cons
                    (cons 'my/expreg-manual-region bounds)
                    (cl-remove-if-not
                     (lambda (range)
                       (and (<= (cadr range) (car bounds))
                            (>= (cddr range) (cdr bounds))))
                     (mapcan (lambda (function)
                               (save-excursion (funcall function)))
                             functions)))))))
          (apply fn args)))))

  (advice-add #'expreg-expand :around #'my/expreg-preserve-manual-region-a))

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

(after! org
  (setq org-ellipsis " ⤵"
        org-edit-src-content-indentation 2)
  ;; Org 自带 Markdown 导出器，首次导出时再加载。
  (add-to-list 'org-export-backends 'md)
  (when (featurep 'ox) (require 'ox-md))
  ;; 两个上游补丁项目的字体族名不同。
  (setopt face-font-family-alternatives
          (cons '("Sarasa Term SC Nerd" "Sarasa Term SC Nerd Font")
                (assoc-delete-all "Sarasa Term SC Nerd"
                                  face-font-family-alternatives)))
  ;; 折叠标记沿用原 Org 字体。
  (custom-set-faces
   '(org-ellipsis ((t (:family "Sarasa Term SC Nerd" :foreground "#E6DC88"))))))

;; Org 的相对行号与普通、可视状态下的 j/k 都按可见屏幕行计数。
(after! org
  (defun my/org-screen-line-numbers-h ()
    "Count visible screen lines in Org without enabling hidden line numbers."
    (setq-local display-line-numbers-type 'visual)
    (when display-line-numbers
      (setq-local display-line-numbers 'visual)))

  (add-hook 'org-mode-hook #'my/org-screen-line-numbers-h)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'org-mode)
        (my/org-screen-line-numbers-h))))

  (map! :after evil
        :map org-mode-map
        :nv "j" #'evil-next-visual-line
        :nv "k" #'evil-previous-visual-line))

(use-package! valign
  :hook (org-mode . valign-mode))

(when (featurep 'neomacs)
  (defun my/neomacs-org-frame-faces-h (&optional frame)
    "Apply missing Org and Outline face specifications to graphical FRAME."
    (let ((frame (or frame (selected-frame))))
      (when (and (featurep 'org-faces) (display-graphic-p frame)
                 (not (ignore-errors (face-attribute 'org-default :inherit frame))))
        (dolist (face (face-list))
          (when (string-match-p (rx string-start (or "org-" "outline-"))
                                (symbol-name face))
            ;; Neomacs 0.0.20 lists lazy faces before they exist on this frame.
            (condition-case nil
                (face-attribute face :foreground frame)
              (error (face-spec-recalc face frame))))))))

  ;; org-mode reads org-default before running any of its mode hooks.
  (after! org
    (advice-add #'org-mode :before #'my/neomacs-org-frame-faces-h))
  (add-hook 'after-make-frame-functions #'my/neomacs-org-frame-faces-h))

;; 各级标题沿用主题颜色和常规字重。
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
  (add-to-list 'evil-escape-excluded-major-modes 'dired-mode)
  (add-to-list 'evil-escape-excluded-states 'visual))

;; 仅在 Evil /、? 的搜索模式构造期间扩展纯小写拼音，保留英文分支。
(after! evil-search
  (defcustom my/evil-pinyin-max-query-length 8
    "Maximum query length for automatic pinyin expansion.
Longer queries keep Evil's original search to avoid expensive pyim regexps."
    :type 'natnum
    :group 'evil)

  (defvar my/evil-pinyin--search-active nil
    "Non-nil while constructing patterns for an Evil / or ? search.")

  (defun my/evil-pinyin--with-search (fn &rest args)
    "Call FN with ARGS while enabling pinyin for Evil search patterns."
    (let ((my/evil-pinyin--search-active t))
      (apply fn args)))

  (defun my/evil-pinyin--expand-pattern (pattern)
    "Expand lowercase searches in PATTERN, preserving Evil's other fields.
Keep the original pattern when pyim is unavailable or conversion fails."
    (save-match-data
      (let ((regexp (car pattern))
            (case-fold-search nil))
        (if (and my/evil-pinyin--search-active
                 (stringp regexp)
                 (<= (length regexp) my/evil-pinyin-max-query-length)
                 (string-match-p "\\`[a-z]+\\'" regexp))
            (condition-case nil
                (if (require 'pyim-cregexp nil t)
                    (let ((expanded (pyim-cregexp-build regexp)))
                      ;; Validate before handing the result to Evil's search/highlighter.
                      (if (and (stringp expanded) (> (length expanded) 0))
                          (progn
                            (string-match-p expanded "")
                            (cons expanded (cdr pattern)))
                        pattern))
                  pattern)
              (error pattern))
          pattern))))

  (advice-add 'evil-ex-search-full-pattern :around #'my/evil-pinyin--with-search)
  (advice-add 'evil-ex-make-search-pattern :filter-return
              #'my/evil-pinyin--expand-pattern))

;; Consult 行搜索等非文件补全也会经过 Doom 中文模块的拼音扩展。
;; 替换其无条件扩展，避免长标识符生成超出 Emacs 限制的正则。
(after! (pyim orderless)
  (defcustom my/orderless-pinyin-max-query-length 8
    "Maximum component length for automatic pinyin completion matching.
Longer components retain their original Orderless regexp."
    :type 'natnum
    :group 'orderless)

  (defun my/orderless-pinyin--expand-regexp (regexp)
    "Expand short lowercase REGEXP for pinyin completion when safe.
Preserve nil, code identifiers and explicit regexps.  Fall back to REGEXP
if expansion fails or exceeds Emacs's regexp compilation limits."
    (save-match-data
      (if (and (stringp regexp)
               (<= (length regexp) my/orderless-pinyin-max-query-length)
               (let ((case-fold-search nil))
                 (string-match-p "\\`[a-z]+\\'" regexp)))
          (condition-case nil
              (let ((expanded (if (modulep! :editor evil +everywhere)
                                  (evil-pinyin--build-regexp-string regexp)
                                (pyim-cregexp-build regexp)))
                    (case-fold-search t))
                (if (and (stringp expanded) (> (length expanded) 0))
                    (progn
                      ;; Orderless 会把 literal 和 regexp 两种匹配合并成 OR。
                      ;; 检查合并结果，覆盖只检查扩展本身时遗漏的临界情况。
                      (string-match-p
                       (rx-to-string
                        `(or (regexp ,(regexp-quote regexp))
                             (regexp ,expanded)) t)
                       "")
                      expanded)
                  regexp))
            (error regexp))
        regexp)))

  ;; 等待 pyim 的 Doom 配置先注册原 advice，保证延迟加载时也能替换。
  (dolist (function '(evil-pinyin--build-regexp-string pyim-cregexp-build))
    (advice-remove 'orderless-regexp function))
  (advice-add 'orderless-regexp :filter-return
              #'my/orderless-pinyin--expand-regexp))

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

(defun my/treat-underscore-as-word ()
  "Treat underscores as part of a word in text buffers."
  (modify-syntax-entry ?_ "w"))

(add-hook 'text-mode-hook #'my/treat-underscore-as-word)

;; 中英文间距由手动编辑，禁用文本模式的自动间距处理。
(remove-hook 'text-mode-hook #'pangu-spacing-mode)

(after! pangu-spacing
  (when (bound-and-true-p global-pangu-spacing-mode)
    (global-pangu-spacing-mode -1))
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (bound-and-true-p pangu-spacing-mode)
        (pangu-spacing-mode -1)))))

(after! pyim
  (defvar gcmh-high-cons-threshold)
  (defvar gcmh-idle-timer)

  (defun my/pyim--with-active-gc-a (function &rest args)
    "Run FUNCTION with ARGS using GCMH's editing threshold during input.
Pyim reads and dispatches keys before the normal command hooks run.
Suspend the pending idle collection until input and candidate selection finish."
    (if (and (bound-and-true-p gcmh-mode)
             (fboundp 'gcmh-register-idle-gc))
        (let ((gc-cons-threshold (max gc-cons-threshold gcmh-high-cons-threshold)))
          (when (timerp gcmh-idle-timer)
            (cancel-timer gcmh-idle-timer)
            (setq gcmh-idle-timer nil))
          (unwind-protect
              (apply function args)
            (when (bound-and-true-p gcmh-mode)
              (gcmh-register-idle-gc))))
      (apply function args)))

  (advice-add 'pyim-input-method :around #'my/pyim--with-active-gc-a)

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
  ;; 基础词库（优先保证常用词可用）
  (when (require 'pyim-basedict nil t)
    (pyim-basedict-enable))
  ;; 清华词库（可选叠加）
  (when (require 'pyim-tsinghua-dict nil t)
    (require 'pyim-dict nil t)
    (pyim-tsinghua-dict-enable))
  ;; 不切输入法也能临时纯英文输入
  (define-key pyim-mode-map (kbd "C-.") #'pyim-toggle-input-ascii))

(defun my/org-pyim-h ()
  "Use Chinese punctuation in Org and ASCII during incremental search."
  (setq-local pyim-english-input-switch-functions '(pyim-probe-isearch-mode)
              pyim-punctuation-half-width-functions nil))

(add-hook 'org-mode-hook #'my/org-pyim-h)

(defun my/org-idle-save-safe-p ()
  "Allow idle saves for modified, existing local plaintext Org documents."
  (and (derived-mode-p 'org-mode)
       buffer-file-name (buffer-modified-p)
       (not (or buffer-read-only (buffer-base-buffer)
                (file-remote-p buffer-file-name)
                (bound-and-true-p epa-file-encrypt-to)
                (string-match-p (if (boundp 'epa-file-name-regexp) epa-file-name-regexp
                                  "\\.gpg\\(?:~\\|\\.~[0-9]+~\\)?\\'")
                                buffer-file-name)))
       (file-regular-p buffer-file-name)
       (not (and (featurep 'org-crypt)
                 (save-restriction
                   (widen)
                   (text-property-not-all (point-min) (point-max) 'org-crypt-text nil))))
       (let ((owner (file-locked-p buffer-file-name)))
         (or (null owner) (eq owner t)))))

(defun my/org-idle-save-preserve-text-a (save buffer)
  "Preserve document text during an idle save of BUFFER."
  (with-current-buffer buffer
    (let ((require-final-newline nil)
          (before-save-hook (remq #'toc-org-insert-toc
                                  (remq #'org-roam-link-replace-all before-save-hook))))
      (funcall save buffer))))

(use-package! super-save
  :after org
  :demand t
  :config
  (when super-save-mode (super-save-mode -1))
  (setq super-save-auto-save-when-idle t
        super-save-idle-duration 30
        super-save-all-buffers t
        super-save-remote-files nil
        super-save-when-focus-lost nil
        super-save-when-buffer-switched nil
        super-save-triggers nil
        super-save-hook-triggers nil
        super-save-handle-org-src nil
        super-save-handle-edit-indirect nil
        super-save-delete-trailing-whitespace nil)
  ;; 此谓词先排除远程文件；包的后续谓词再检查可写权限和磁盘版本。
  (add-hook 'super-save-predicates #'my/org-idle-save-safe-p -90)
  (advice-add #'super-save-buffer :around #'my/org-idle-save-preserve-text-a)
  (super-save-mode 1))

;;; config.el ends here
