;;; local/environment/+service.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defun +local-env--directory (directory)
  "Normalize a local DIRECTORY without invoking an external tool."
  (file-name-as-directory (expand-file-name directory)))

(defun +local-env--capture-base ()
  "Capture Doom's machine-local default environment, never a file buffer's."
  (setq +local-env--base-environment
        (+local-env--safe-environment (default-value 'process-environment))
        +local-env--base-exec-path (copy-sequence (default-value 'exec-path))))

(defun +local-env--safe-environment (environment)
  "Copy ENVIRONMENT with implicit tool downloads and mise networking disabled."
  (let ((process-environment (copy-sequence environment)))
    (dolist (setting '(("MISE_AUTO_INSTALL" . "0") ("MISE_EXEC_AUTO_INSTALL" . "0")
                       ("MISE_NOT_FOUND_AUTO_INSTALL" . "0")
                       ("MISE_NOT_FOUND_SYSTEM_FALLBACK" . "0")
                       ("MISE_OFFLINE" . "1") ("MISE_UPDATE_CHECK" . "0")
                       ("MISE_ENV_CACHE" . "0") ("RUSTUP_AUTO_INSTALL" . "0")))
      (setenv (car setting) (cdr setting)))
    process-environment))

(defun +local-env--ancestors (directory)
  "List DIRECTORY and its ancestors, nearest first."
  (let (result parent)
    (while directory
      (push directory result)
      (setq parent (file-name-directory (directory-file-name directory))
            directory (unless (equal directory parent) parent)))
    (nreverse result)))

(defun +local-env--global-paths ()
  "Return dynamically discovered global mise configuration locations."
  (let* ((process-environment +local-env--base-environment)
         (config (or (getenv "MISE_CONFIG_DIR")
                     (expand-file-name "mise" (or (getenv "XDG_CONFIG_HOME") "~/.config"))))
         (system (or (getenv "MISE_SYSTEM_CONFIG_DIR") "/etc/mise")))
    (cl-remove-if
     #'file-remote-p
     (delq nil
          (append (mapcar (lambda (name) (expand-file-name name config))
                          '("config.toml" "miserc.toml" "conf.d/"))
                  (mapcar (lambda (name) (expand-file-name name system))
                          '("config.toml" "miserc.toml" "conf.d/"))
                  (mapcar (lambda (name) (when-let ((path (getenv name)))
                                          (expand-file-name path)))
                          '("MISE_CONFIG_FILE" "MISE_GLOBAL_CONFIG_FILE" "MISE_ENV_FILE")))))))

(defconst +local-env--config-name-regexp
  "\\`\\(?:\\.?mise.*\\.\\(?:toml\\|lock\\)\\|\\.miserc\\.toml\\|\\.tool-versions\\|\\.nvmrc\\|\\.node-version\\|\\.python-version\\|\\.ruby-version\\|rust-toolchain\\(?:\\.toml\\)?\\|package\\.json\\)\\'"
  "Files capable of defining or selecting a component's tool environment.")

(defun +local-env--candidates (directory)
  "Discover configuration candidates and notification directories for DIRECTORY."
  (let (files folders)
    (dolist (ancestor (+local-env--ancestors directory))
      (dolist (relative '("" ".mise/" ".config/mise/" ".mise/conf.d/" ".config/mise/conf.d/"))
        (let ((folder (expand-file-name relative ancestor)))
          (push folder folders)
          (when (file-directory-p folder)
            (setq files
                  (append (ignore-errors
                            (directory-files folder t
                                             (concat "\\(?:" +local-env--config-name-regexp "\\|\\.toml\\'\\)") t))
                          files))))))
    (dolist (path (+local-env--global-paths))
      (if (file-directory-p path)
          (progn (push path folders)
                 (setq files (append (ignore-errors (directory-files path t "\\.toml\\'" t)) files)))
        (push path files)
        (push (file-name-directory path) folders)))
    (list :files (sort (delete-dups
                       (mapcar #'file-truename (cl-remove-if #'file-remote-p files))) #'string<)
          :folders (delete-dups folders))))

(defun +local-env--references (files)
  "Find literal dotenv/source/include references without evaluating config."
  (let (references dynamic)
    (dolist (file files)
      (when (and (string-suffix-p ".toml" file) (file-readable-p file)
                 (file-regular-p file))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          ;; Cwd-sensitive templates and arbitrary commands cannot share results
          ;; safely across source directories, even with identical config paths.
          (when (re-search-forward "\\bcwd\\b\\|\\bpwd\\b\\|\\bsource\\s-*=\\|\\bexec\\s-*(\\|override_\\(?:config\\|tool_versions\\)_filenames" nil t)
            (setq dynamic t))
          (goto-char (point-min))
          (while (re-search-forward
                  "\\(?:\\`\\|[[:space:].]\\)\\(?:file\\|source\\|env_file\\|includes?\\)\\s-*=\\s-*\\(\\[[^]]*\\]\\|[^\n]+\\)" nil t)
            (let ((value (match-string-no-properties 1)) (start 0))
              (while (string-match "[\"']\\([^\"']+\\)[\"']" value start)
                (let* ((name (match-string 1 value))
                       (path (expand-file-name name (file-name-directory file))))
                  (unless (or (file-remote-p path) (string-match-p "{{\\|\\${" name))
                    (setq path (file-truename path))
                    (push path references)
                    (when (string-match-p "[*?]" path)
                      (setq references (append (file-expand-wildcards path t) references)))))
                (setq start (match-end 0))))))))
    (list :files (delete-dups references) :dynamic dynamic)))

(defun +local-env--fingerprint (files)
  "Return file metadata; values from dotenv files are never read or logged."
  (mapcar
   (lambda (file)
     (list file (when-let ((attrs (and (not (file-remote-p file))
                                      (ignore-errors (file-attributes file 'integer)))))
                  (list (file-attribute-type attrs) (file-attribute-size attrs)
                        (file-attribute-modification-time attrs)
                        (file-attribute-status-change-time attrs)))))
   (sort (delete-dups (copy-sequence files)) #'string<)))

(defun +local-env--descriptor (directory)
  "Describe DIRECTORY's effective candidate context without invoking mise."
  (let* ((candidates (+local-env--candidates directory))
         (files (plist-get candidates :files))
         (references (+local-env--references files))
         (exec-path +local-env--base-exec-path)
         (mise (+local-env-executable-find "mise"))
         (base-key (secure-hash 'sha256
                                (prin1-to-string (list +local-env--base-environment
                                                      +local-env--base-exec-path))))
         (key (list mise base-key files
                    (when (or (plist-get references :dynamic)
                              (let ((process-environment +local-env--base-environment))
                                (or (getenv "MISE_OVERRIDE_CONFIG_FILENAMES")
                                    (getenv "MISE_OVERRIDE_TOOL_VERSIONS_FILENAMES"))))
                      (file-truename directory)))))
    (list :key key :directory directory :mise mise :files files
          :reference-files (plist-get references :files)
          :folders (plist-get candidates :folders)
          :fingerprint (+local-env--fingerprint
                        (append files (plist-get references :files))))))

(defun +local-env--describe (directory)
  "Describe DIRECTORY safely; inaccessible configuration never blocks editing."
  (condition-case nil (+local-env--descriptor directory)
    (error (list :key (list 'unreadable directory) :directory directory
                 :mise nil :files nil :folders nil :fingerprint nil
                 :error "无法读取项目环境配置；修复文件权限后环境会自动恢复"))))

(defun +local-env--json (buffer)
  "Read BUFFER's JSON, discarding parse errors rather than exposing values."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((json-object-type 'alist) (json-array-type 'list)
          (json-key-type 'string) (json-false :false) (json-null nil))
      (let ((value (json-read)))
        (skip-chars-forward " \t\r\n")
        (unless (eobp) (error "Invalid JSON response"))
        value))))

(defun +local-env--query (program args directory callback)
  "Run one fully asynchronous JSON query and call CALLBACK with OK and JSON.
Return a cancellation function.  Output buffers are always private and removed;
no stdout, stderr, command arguments, or environment values appear in errors."
  (let ((stdout (generate-new-buffer " *local-environment-output*"))
        (stderr (generate-new-buffer " *local-environment-error*"))
        (default-directory directory)
        (process-environment (+local-env--safe-environment +local-env--base-environment))
        (exec-path +local-env--base-exec-path)
        process timer done)
    (cl-labels
        ((cleanup ()
           (when (timerp timer) (cancel-timer timer))
           (dolist (buffer (list stdout stderr))
             (when (buffer-live-p buffer)
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))))
         (finish (ok data)
           (unless done
             (setq done t)
             (cleanup)
             (funcall callback ok data))))
      (condition-case nil
          (progn
            (setq process
                  (make-process
                   :name "local-environment" :buffer stdout :stderr stderr
                   :command (cons program args) :coding 'utf-8-unix
                   :connection-type 'pipe :noquery t
                   :sentinel
                   (lambda (proc _event)
                     (when (and (not done) (memq (process-status proc) '(exit signal)))
                       (if (and (eq (process-status proc) 'exit)
                                (zerop (process-exit-status proc)))
                           (condition-case nil (finish t (+local-env--json stdout))
                             (error (finish nil nil)))
                         (finish nil nil))))))
            (setq timer
                  (run-with-timer +local-env-command-timeout nil
                                  (lambda ()
                                    (unless done
                                      (setq done t)
                                      (when (process-live-p process) (delete-process process))
                                      (cleanup)
                                      (funcall callback nil nil))))))
        (error (finish nil nil)))
      (lambda ()
        (unless done
          (setq done t)
          (when (and process (process-live-p process)) (delete-process process))
          (cleanup))))))

(defun +local-env--merge (entries)
  "Merge mise's string-keyed environment entries into the machine baseline."
  (let ((process-environment (copy-sequence +local-env--base-environment)))
    (dolist (entry entries)
      (unless (and (consp entry) (stringp (car entry))
                   (string-match-p "\\`[[:alnum:]_]+\\'" (car entry))
                   (or (null (cdr entry)) (stringp (cdr entry))))
        (error "Invalid environment response"))
      (setenv (car entry) (cdr entry)))
    (+local-env--safe-environment process-environment)))

(defun +local-env--tools (entries directory)
  "Normalize selected mise tool metadata without exposing configuration values."
  (let (tools)
    (dolist (entry entries)
      (unless (and (consp entry) (stringp (car entry)) (listp (cdr entry)))
        (error "Invalid tool response"))
      (dolist (version (cdr entry))
        (let ((number (alist-get "version" version nil nil #'equal))
              (source (alist-get "path" (alist-get "source" version nil nil #'equal) nil nil #'equal))
              (installation (alist-get "install_path" version nil nil #'equal)))
          (unless (and (stringp number) (or (null source) (stringp source))
                       (or (null installation) (stringp installation)))
            (error "Invalid tool version"))
          (push (list :name (car entry) :version number
                      :source (when source (file-truename (expand-file-name source directory)))
                      :install-directory (when installation
                                           (+local-env--directory (expand-file-name installation directory)))
                      :installed (eq (alist-get "installed" version nil nil #'equal) t))
                tools))))
    (nreverse tools)))

(defun +local-env--copy-context (context directory)
  "Copy a public CONTEXT for its actual caller DIRECTORY."
  (let ((copy (copy-sequence context)))
    (setq copy (plist-put copy :directory directory))
    (setq copy (plist-put copy :process-environment
                          (copy-sequence (plist-get context :process-environment))))
    (setq copy (plist-put copy :tools (copy-tree (plist-get context :tools))))
    (plist-put copy :exec-path (copy-sequence (plist-get context :exec-path)))))

(defun +local-env--semantic-context (context)
  "Return state affecting processes and selected tool versions."
  (list (plist-get context :status) (plist-get context :process-environment)
        (plist-get context :exec-path) (plist-get context :tools)))

(defun +local-env--subscribe (key directory)
  "Attach the current file buffer when DIRECTORY is its real component directory."
  (let ((actual (+local-env--directory
                 (if buffer-file-name (file-name-directory buffer-file-name) default-directory))))
    (when (and buffer-file-name (equal actual directory))
      (when-let ((old-key +local-env--buffer-key))
        (unless (equal old-key key)
          (when-let ((old (gethash old-key +local-env--cache)))
            (setf (plist-get old :buffers) (delq (current-buffer) (plist-get old :buffers))))))
      (setq-local +local-env--buffer-directory directory
                  +local-env--buffer-key key)
      (let ((record (gethash key +local-env--cache)))
        (cl-pushnew (current-buffer) (plist-get record :buffers))))))

(defun +local-env--apply (record)
  "Apply RECORD to its subscribed buffers and emit only meaningful changes."
  (let ((context (plist-get record :context)))
    (dolist (buffer (copy-sequence (plist-get record :buffers)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (when (and (equal +local-env--buffer-key (plist-get record :key))
                     (equal (gethash +local-env--buffer-directory +local-env--directories)
                            (plist-get record :key)))
            (let ((new (+local-env--copy-context context +local-env--buffer-directory))
                  (old +local-env--applied-context))
              (unless old
                (setq-local +local-env--native-environment (copy-sequence process-environment)
                            +local-env--native-exec-path (copy-sequence exec-path)
                            +local-env--native-local-environment-p (local-variable-p 'process-environment)
                            +local-env--native-local-exec-path-p (local-variable-p 'exec-path)))
              (setq-local process-environment (copy-sequence (plist-get new :process-environment))
                          exec-path (copy-sequence (plist-get new :exec-path))
                          +local-env--applied-context new)
              (unless (equal (+local-env--semantic-context old) (+local-env--semantic-context new))
                (condition-case nil
                    (run-hook-with-args '+local-env-changed-hook old new)
                  (error (message "项目环境消费者未能完成刷新；可手动重试")))))))))))

(defun +local-env--deliver (callbacks context)
  "Deliver CALLBACKS in their still-live originating buffers."
  (dolist (entry callbacks)
    (pcase-let ((`(,buffer ,directory ,function) entry))
      (when (and function (buffer-live-p buffer))
        (with-current-buffer buffer
          (condition-case nil
              (funcall function (+local-env--copy-context context directory))
            (error (message "项目环境回调未能完成；可手动重试"))))))))

(defun +local-env--settle (key request status &optional entries reason)
  "Publish only a current REQUEST whose configuration has not changed."
  (when (eq request (gethash key +local-env--requests))
    (let* ((record (gethash key +local-env--cache))
           (directory (plist-get request :directory))
           (fresh (+local-env--describe directory))
           (sources (plist-get request :sources))
           (files (delete-dups (append (plist-get fresh :files) sources
                                       (plist-get (+local-env--references sources) :files))))
           (current (+local-env--fingerprint files)))
      (if (or (not (equal key (plist-get fresh :key)))
              (not (equal (plist-get request :fingerprint)
                          (+local-env--fingerprint (plist-get request :watch-files)))))
          ;; A save/notification raced with the process.  Requeue consumers under
          ;; the fresh context rather than briefly applying stale data.
          (let ((callbacks (plist-get request :callbacks)))
            (remhash key +local-env--requests)
            (dolist (buffer (plist-get record :buffers))
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (+local-env--ensure +local-env--buffer-directory nil t))))
            (dolist (entry callbacks)
              (when (buffer-live-p (car entry))
                (with-current-buffer (car entry)
                  (+local-env--ensure (nth 1 entry) (nth 2 entry) nil)))))
        (let* ((environment (if (eq status 'ready)
                                (condition-case nil (+local-env--merge entries)
                                  (error nil))
                              +local-env--base-environment))
               (paths (if environment
                          (let ((process-environment environment))
                            (append (parse-colon-path (or (getenv "PATH") "")) (list exec-directory)))
                        +local-env--base-exec-path))
               (old (plist-get record :settled))
               (context (list :directory directory
                              :status (if environment status 'unavailable)
                              :reason (if environment reason "mise 环境响应无效")
                              :process-environment (or environment +local-env--base-environment)
                              :exec-path paths
                              :tools (copy-tree (plist-get request :tools))
                              :generation (or (plist-get old :generation) 0))))
          (unless (equal (+local-env--semantic-context old) (+local-env--semantic-context context))
            (setf (plist-get context :generation) (cl-incf +local-env--generation)))
          (setf (plist-get record :context) context
                (plist-get record :settled) context
                (plist-get record :sources) sources
                (plist-get record :watch-files) files
                (plist-get record :fingerprint) current
                (plist-get record :time) (float-time)
                (plist-get record :descriptor) fresh)
          (remhash key +local-env--requests)
          (when (fboundp '+local-env--watch-record) (+local-env--watch-record key record))
          (+local-env--apply record)
          (when (eq (plist-get context :status) 'unavailable)
            (unless (gethash (list key (plist-get context :reason)) +local-env--notices)
              (puthash (list key (plist-get context :reason)) t +local-env--notices)
              (message "项目环境暂不可用：%s；编辑和保存仍可继续" (plist-get context :reason))))
          (when (memq (plist-get context :status) '(ready unmanaged))
            (let (resolved)
              (maphash (lambda (notice _)
                         (when (equal (car notice) key) (push notice resolved))) +local-env--notices)
              (dolist (notice resolved) (remhash notice +local-env--notices))))
          (+local-env--deliver (plist-get request :callbacks) context))))))

(defun +local-env--start (key request descriptor)
  "Resolve sources, missing tools and environment with chained async queries."
  (let ((mise (plist-get descriptor :mise))
        (directory (plist-get descriptor :directory)))
    (cl-labels
        ((query (args callback)
           (when (eq request (gethash key +local-env--requests))
             (setf (plist-get request :cancel)
                   (+local-env--query mise args directory
                                      (lambda (ok data)
                                        (when (eq request (gethash key +local-env--requests))
                                          (condition-case nil (funcall callback ok data)
                                            (error (unavailable)))))))))
         (unavailable ()
           (+local-env--settle key request 'unavailable nil
                               "mise 解析失败；请检查本机工具版本或项目配置的信任状态"))
         (tools (ok data)
           (when ok
             (setf (plist-get request :tools) (+local-env--tools data directory)))
           (cond ((not ok) (unavailable))
                 ((cl-some (lambda (tool) (not (plist-get tool :installed)))
                           (plist-get request :tools))
                  (+local-env--settle key request 'unavailable nil
                                      "项目声明的工具版本未安装；安装后环境会自动恢复"))
                 (t (query '("env" "--json")
                           (lambda (success data)
                             (if success (+local-env--settle key request 'ready data)
                               (unavailable)))))))
         (sources (ok data)
           (let* ((paths (if ok
                             (delq nil (mapcar (lambda (entry)
                                                 (when-let ((path (and (listp entry)
                                                                       (alist-get "path" entry nil nil #'equal))))
                                                   (let ((file (expand-file-name path directory)))
                                                     (unless (file-remote-p file) (file-truename file))))) data))
                           (cl-remove-if-not #'file-regular-p (plist-get descriptor :files))))
                  (files (delete-dups (append (plist-get descriptor :files) paths
                                              (plist-get (+local-env--references paths) :files)))))
             (setf (plist-get request :sources) paths
                   (plist-get request :watch-files) files
                   (plist-get request :fingerprint) (+local-env--fingerprint files))
             (query '("ls" "--current" "--json") #'tools))))
      (cond
       ((plist-get descriptor :error)
        (+local-env--settle key request 'unavailable nil (plist-get descriptor :error)))
       (mise (query '("config" "ls" "--json") #'sources))
       (t
        (let ((declared (cl-some
                         (lambda (file)
                           (and (file-regular-p file)
                                (string-match-p "mise.*\\.toml\\|\\.tool-versions\\'" file)))
                         (plist-get descriptor :files))))
          (+local-env--settle key request (if declared 'unavailable 'unmanaged) nil
                              (when declared "未找到 mise；请更新本机 Doom 环境缓存或本机工具覆盖"))))))))

(defun +local-env--ensure (directory callback force)
  "Internal asynchronous service entrypoint for DIRECTORY."
  (setq directory (+local-env--directory directory))
  (if (file-remote-p directory)
      (let ((context (+local-env-context directory)))
        (when callback (funcall callback context))
        context)
    (unless +local-env--base-environment (+local-env--capture-base))
    (let* ((descriptor (+local-env--describe directory))
           (key (plist-get descriptor :key))
           (record (gethash key +local-env--cache))
           (request (gethash key +local-env--requests))
           (watch-files (or (plist-get record :watch-files)
                            (append (plist-get descriptor :files)
                                    (plist-get descriptor :reference-files))))
           (fingerprint (+local-env--fingerprint watch-files))
           (entry (when callback (list (current-buffer) directory callback))))
      (unless record
        (setq record (list :key key :context nil :settled nil :buffers nil :directories nil
                           :sources nil :watch-files nil :fingerprint nil
                           :descriptor descriptor :time 0))
        (puthash key record +local-env--cache))
      (cl-pushnew directory (plist-get record :directories) :test #'equal)
      (puthash directory key +local-env--directories)
      (+local-env--subscribe key directory)
      (cond
       ((and request (not force))
        (when entry (push entry (plist-get request :callbacks))))
       ((and (not force) (plist-get record :settled)
             (equal fingerprint (plist-get record :fingerprint))
             (< (- (float-time) (plist-get record :time))
                (if (eq (plist-get (plist-get record :settled) :status) 'unavailable)
                    +local-env-retry-interval +local-env-cache-ttl)))
        (when (fboundp '+local-env--watch-record) (+local-env--watch-record key record))
        (+local-env--apply record)
        (when entry (+local-env--deliver (list entry) (plist-get record :context))))
       (t
        (let ((callbacks (append (when entry (list entry)) (plist-get request :callbacks))))
          (when-let ((cancel (plist-get request :cancel))) (funcall cancel))
          ;; Adding a plist key can change identity and invalidate callback guards.
          (setq request (list :serial (cl-incf +local-env--serial) :directory directory
                              :callbacks callbacks :sources nil :tools nil :cancel nil
                              :watch-files watch-files :fingerprint fingerprint))
          (puthash key request +local-env--requests)
          (setf (plist-get record :context)
                (plist-put (copy-sequence (or (plist-get record :settled)
                                              (+local-env-context directory))) :status 'pending))
          (+local-env--start key request descriptor))))
      (+local-env-context directory))))
