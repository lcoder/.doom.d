;;; +file-location.el -*- lexical-binding: t; -*-

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
