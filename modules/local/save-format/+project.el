;;; +project.el --- Project formatter selection -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defun +local-save-format--read-json (file)
  "Read FILE as string-keyed alists, without executing project configuration."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (let ((json-object-type 'alist) (json-array-type 'list)
            (json-key-type 'string) (json-null nil) (json-false :false))
        (json-read)))))

(defun +local-save-format--adapter (tool)
  (cdr (assq tool +local-save-format-adapters)))

(defun +local-save-format--script-tool-p (script tool)
  "Whether formatter SCRIPT names TOOL as a command, rather than a substring."
  (and (stringp script)
       (string-match-p
        (concat "\\(?:\\`\\|[;|&]\\)[[:space:]]*"
                "\\(?:[[:alnum:]_]+=[^[:space:]]+[[:space:]]+\\)*"
                "\\(?:\\(?:npx\\|bunx\\)[[:space:]]+\\|\\(?:pnpm\\|yarn\\|bun\\)[[:space:]]+\\(?:exec[[:space:]]+\\)?\\)?"
                (regexp-quote (plist-get (+local-save-format--adapter tool) :executable))
                "\\(?:[[:space:];|&]\\|\\'\\)")
        script)))

(defun +local-save-format--script-evidence (package tool)
  (cl-some (lambda (entry)
             (and (string-match-p "\\`\\(?:fmt\\|format\\)\\(?:\\'\\|[:_-]\\)"
                                  (car entry))
                  (+local-save-format--script-tool-p (cdr entry) tool)))
           (alist-get "scripts" package nil nil #'equal)))

(defun +local-save-format--config-evidence (directory package tool)
  (let ((adapter (+local-save-format--adapter tool)))
    (or (cl-some (lambda (file)
                   (file-exists-p (expand-file-name file directory)))
                 (plist-get adapter :config-files))
        (when-let ((key (plist-get adapter :package-key)))
          (assoc key package))
        (+local-save-format--script-evidence package tool))))

(defun +local-save-format--dependency-evidence (package tool)
  (let ((name (plist-get (+local-save-format--adapter tool) :package)))
    (cl-some (lambda (key)
               (assoc name (alist-get key package nil nil #'equal)))
             '("dependencies" "devDependencies"))))

(defun +local-save-format--mise-tool (tool)
  "Return TOOL's cached project mise declaration through the public environment API."
  (when (and (+local-save-format--env-p) (fboundp '+local-env-project-tool))
    (+local-env-project-tool (concat "npm:" (plist-get (+local-save-format--adapter tool) :package))
                             (+local-save-format--source-directory))))

(defun +local-save-format-js-project ()
  "Resolve explicit JS formatter rules, direct dependencies, then mise declarations.
At a given directory all evidence at the same priority must agree.  A nearer
package's direct dependencies precede those of an enclosing workspace, while
an explicit enclosing formatter config still precedes dependency inference."
  (when (and buffer-file-name
             (string-match-p
              "\\.\\(?:[cm]?[jt]sx?\\|json[c5]?\\|css\\|scss\\|less\\|html\\|mdx?\\|ya?ml\\|svelte\\|vue\\)\\'"
              buffer-file-name))
    (let* ((directories (+local-save-format--directories (+local-save-format--source-directory)))
           (packages (mapcar (lambda (directory)
                               (cons directory
                                     (+local-save-format--read-json
                                      (expand-file-name "package.json" directory))))
                             directories))
           result)
      (catch 'found
        (dolist (priority '(config dependency mise))
          (dolist (entry packages)
            (let ((tools
                   (cl-loop for (tool . _adapter) in +local-save-format-adapters
                            when (pcase priority
                                   ('config (+local-save-format--config-evidence
                                             (car entry) (cdr entry) tool))
                                   ('dependency (+local-save-format--dependency-evidence (cdr entry) tool))
                                   ('mise (equal (car entry)
                                                 (plist-get (+local-save-format--mise-tool tool) :directory))))
                            collect tool)))
              (when tools
                (setq result
                      (if (cdr tools)
                          (list :blocked t :reason
                                (format "多个格式器声明冲突（%s）；已普通保存，请设置 apheleia-formatter/+format-with。"
                                        (mapconcat #'symbol-name tools ", ")))
                        (list :formatters tools :directory (car entry))))
                (throw 'found result)))))
        nil)
      result)))

(defun +local-save-format--file-signatures (files)
  (mapcar (lambda (file)
            (let ((attributes (file-attributes file 'string)))
              (list file (and attributes (file-attribute-modification-time attributes))
                    (and attributes (file-attribute-size attributes)))))
          (delete-dups files)))

(defun +local-save-format--rust-edition ()
  "Resolve this buffer's effective Cargo package or exact target edition."
  (let* ((file (file-truename buffer-file-name))
         (metadata (+local-save-format--cargo-metadata default-directory))
         (packages
          (sort (cl-remove-if-not
                 (lambda (package)
                   (when-let ((manifest (alist-get "manifest_path" package nil nil #'equal)))
                     (file-in-directory-p file (file-name-directory manifest))))
                 (copy-sequence (alist-get "packages" metadata nil nil #'equal)))
                (lambda (a b)
                  (> (length (alist-get "manifest_path" a nil nil #'equal))
                     (length (alist-get "manifest_path" b nil nil #'equal))))))
         (package (car packages))
         (targets (alist-get "targets" package nil nil #'equal))
         (target (cl-find-if
                  (lambda (target)
                    (when-let ((path (alist-get "src_path" target nil nil #'equal)))
                      (equal (file-truename path) file)))
                  targets))
         (matching-targets
          (sort (cl-remove-if-not
                 (lambda (target)
                   (when-let ((path (alist-get "src_path" target nil nil #'equal)))
                     (file-in-directory-p file (file-name-directory path))))
                 (copy-sequence targets))
                (lambda (a b)
                  (> (length (file-name-directory (alist-get "src_path" a nil nil #'equal)))
                     (length (file-name-directory (alist-get "src_path" b nil nil #'equal)))))))
         (nearest-length (and matching-targets
                              (length (file-name-directory
                                       (alist-get "src_path" (car matching-targets) nil nil #'equal)))))
         (target-editions
          (delete-dups
           (cl-loop for candidate in matching-targets
                    when (= nearest-length
                            (length (file-name-directory
                                     (alist-get "src_path" candidate nil nil #'equal))))
                    collect (alist-get "edition" candidate nil nil #'equal))))
         (edition (or (alist-get "edition" target nil nil #'equal)
                      (and (not (cdr target-editions)) (car target-editions))
                      (and (not target-editions)
                           (alist-get "edition" package nil nil #'equal)))))
    (unless (and (stringp edition) (string-match-p "\\`[0-9]\\{4\\}\\'" edition))
      (error "无法可靠确定当前文件的 Cargo edition"))
    edition))

(defun +local-save-format--rust-config-directory ()
  "Return the nearest Rust formatter configuration directory."
  (cl-loop for directory in (+local-save-format--directories (+local-save-format--source-directory))
           when (cl-some (lambda (name)
                           (file-exists-p (expand-file-name name directory)))
                         '("rustfmt.toml" ".rustfmt.toml"))
           return directory))

(defun +local-save-format-tool-executable (tool)
  "Find only an already installed project TOOL; never install or download it.
Oxfmt and Biome use project dependencies or explicitly declared mise tools.
Prettier may use an installed language default from PATH when no project
dependency is present."
  (let* ((adapter (+local-save-format--adapter tool))
         (name (plist-get adapter :executable))
         (local (cl-loop for directory in (+local-save-format--directories
                                           (+local-save-format--source-directory))
                         for candidate = (expand-file-name
                                          (concat "node_modules/.bin/" name) directory)
                         when (file-executable-p candidate) return candidate))
         (mise (when (and (+local-save-format--env-p)
                          (fboundp '+local-env-project-tool-executable))
                 (+local-env-project-tool-executable
                  (concat "npm:" (plist-get adapter :package)) name
                  (+local-save-format--source-directory))))
         (global (and (eq tool 'prettier)
                      (plist-get +local-save-format--selection :language-default)
                      (not (+local-save-format--project-declares-tool-p tool))
                      (executable-find name))))
    (or local mise global (error "项目中未安装 %s；已保留普通保存内容" name))))

(defun +local-save-format--project-declares-tool-p (tool)
  "Whether any applicable project directory explicitly declares TOOL."
  (or (+local-save-format--mise-tool tool)
      (cl-some (lambda (directory)
                 (let ((package (+local-save-format--read-json
                                 (expand-file-name "package.json" directory))))
                   (or (+local-save-format--config-evidence directory package tool)
                       (+local-save-format--dependency-evidence package tool))))
               (+local-save-format--directories (+local-save-format--source-directory)))))

(defun +local-save-format--source-directory ()
  "Return the actual file's directory, independently of formatter working cwd."
  (if buffer-file-name (file-name-directory buffer-file-name) default-directory))

(defun +local-save-format--script-options (tool)
  "Retain config/ignore file flags from a simple formatter declaration.
Only known path flags are read; scripts, shell expressions and bulk paths are
never executed or passed to the single-file formatter."
  (when-let* ((selection +local-save-format--selection)
              (directory (plist-get selection :directory))
              (package (+local-save-format--read-json (expand-file-name "package.json" directory)))
              (script (cl-loop for (name . script) in (alist-get "scripts" package nil nil #'equal)
                               when (and (member name '("format" "fmt"))
                                         (+local-save-format--script-tool-p script tool))
                               return script)))
    (let ((tokens (condition-case nil (split-string-and-unquote script) (error nil)))
          options)
      (while tokens
        (let ((token (pop tokens)))
          (when (member token '("--config" "-c" "--ignore-path"))
            (let ((value (pop tokens)))
              (when (and value
                         (not (string-match-p "[;$`|&<>]" value))
                         (file-exists-p (expand-file-name value directory)))
                (setq options (append options (list token value))))))))
      options)))

