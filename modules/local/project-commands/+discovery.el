;;; local/project-commands/+discovery.el -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst +local-project--markers
  '("package.json" "Cargo.toml" "pubspec.yaml" "Justfile" "justfile" ".justfile" "mise.toml" ".mise.toml"
    "pnpm-workspace.yaml" "pnpm-lock.yaml" "yarn.lock" "package-lock.json" "npm-shrinkwrap.json" "bun.lock" "bun.lockb"))
(defun +local-project--environment-p ()
  (and (modulep! :local environment) (fboundp '+local-env-context)))
(defun +local-project--directory (&optional directory)
  (let ((directory (file-name-as-directory (expand-file-name (or directory default-directory)))))
    (or (locate-dominating-file directory
                                (lambda (path)
                                  (cl-some (lambda (name) (file-exists-p (expand-file-name name path)))
                                           '("package.json" "Cargo.toml" "pubspec.yaml" "Justfile" "justfile" ".justfile"))))
        (locate-dominating-file directory
                                (lambda (path)
                                  (or (file-exists-p (expand-file-name "mise.toml" path))
                                      (file-exists-p (expand-file-name ".mise.toml" path)))))
        (locate-dominating-file directory ".git") directory)))

(defun +local-project--json-file (file)
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string)
            (json-false nil) (json-null nil))
        (json-read)))))

(defun +local-project--workspace-pattern-p (pattern relative)
  "Match common workspace path globs without treating star as a path separator."
  (when (stringp pattern)
    (let ((index 0) (regexp "") (length (length pattern)))
      (while (< index length)
        (let ((char (aref pattern index)))
          (cond
           ((and (= char ?*) (< (1+ index) length) (= (aref pattern (1+ index)) ?*))
            (if (and (< (+ index 2) length) (= (aref pattern (+ index 2)) ?/))
                (setq regexp (concat regexp "\\(?:[^/]+/\\)*") index (+ index 3))
              (setq regexp (concat regexp ".*") index (+ index 2))))
           ((= char ?*) (setq regexp (concat regexp "[^/]*") index (1+ index)))
           ((= char ??) (setq regexp (concat regexp "[^/]") index (1+ index)))
           (t (setq regexp (concat regexp (regexp-quote (char-to-string char))) index (1+ index))))))
      (string-match-p (concat "\\`" regexp "\\'") relative))))

(defun +local-project--workspace-root (directory)
  "Find a declared workspace containing DIRECTORY, never a plain parent project."
  (let ((parent (file-name-directory (directory-file-name directory)))
        (boundary (locate-dominating-file directory ".git")) found)
    (while (and parent (not found)
                (or (null boundary) (equal parent boundary) (file-in-directory-p parent boundary)))
      (let* ((package (+local-project--json-file (expand-file-name "package.json" parent)))
             (workspaces (alist-get "workspaces" package nil nil #'equal))
             (patterns (if (and (listp workspaces) (assoc "packages" workspaces))
                           (alist-get "packages" workspaces nil nil #'equal) workspaces))
             (relative (directory-file-name (file-relative-name directory parent))))
        (when (or (file-exists-p (expand-file-name "pnpm-workspace.yaml" parent))
                  (and (listp patterns)
                       (cl-some (lambda (pattern)
                                  (and (stringp pattern) (not (string-prefix-p "!" pattern))
                                       (+local-project--workspace-pattern-p pattern relative))) patterns)
                       (not (cl-some (lambda (pattern)
                                       (and (stringp pattern) (string-prefix-p "!" pattern)
                                            (+local-project--workspace-pattern-p (substring pattern 1) relative))) patterns))))
          (setq found parent)))
      (let ((next (file-name-directory (directory-file-name parent))))
        (setq parent (unless (equal next parent) next))))
    found))

(defun +local-project--lock-managers (directory)
  "Read only lockfile declarations in DIRECTORY."
  (cl-loop for (name . files) in
           '(("pnpm" "pnpm-lock.yaml") ("yarn" "yarn.lock")
             ("bun" "bun.lock" "bun.lockb") ("npm" "package-lock.json" "npm-shrinkwrap.json"))
           when (cl-some (lambda (file) (file-exists-p (expand-file-name file directory))) files)
           collect name))

(defun +local-project--manager (package directory)
  "Prefer local declarations; inherit a manager only from an explicit workspace."
  (let* ((locks (+local-project--lock-managers directory))
         (workspace (and (not (alist-get "packageManager" package nil nil #'equal))
                         (null locks) (+local-project--workspace-root directory)))
         (workspace-package (and workspace (+local-project--json-file (expand-file-name "package.json" workspace))))
         (declared (or (alist-get "packageManager" package nil nil #'equal)
                       (alist-get "packageManager" workspace-package nil nil #'equal)))
         (manager (and (stringp declared) (car (split-string declared "@")))))
    (when (and (null locks) workspace) (setq locks (+local-project--lock-managers workspace)))
    (cond ((member manager '("npm" "pnpm" "yarn" "bun")) manager)
          (declared nil) ((= (length locks) 1) (car locks))
          ((null locks) "npm"))))

(defun +local-project--shell (args)
  (mapconcat #'shell-quote-argument args " "))

(defun +local-project--read-defaults (directory)
  "Read existing declarations.  Never run scripts or install dependencies."
  (cond
   ((file-exists-p (expand-file-name "package.json" directory))
    (let* ((package (+local-project--json-file (expand-file-name "package.json" directory)))
           (manager (+local-project--manager package directory))
           (scripts (alist-get "scripts" package nil nil #'equal)))
      (when manager
        (cl-loop for (phase . names) in '((build "build") (test "test") (run "dev" "start"))
                 for name = (cl-find-if (lambda (name) (stringp (alist-get name scripts nil nil #'equal))) names)
                 when name collect (cons phase (+local-project--shell (list manager "run" name)))))))
   ((file-exists-p (expand-file-name "Cargo.toml" directory))
    '((build . "cargo build") (test . "cargo test") (run . "cargo run")))
   ((file-exists-p (expand-file-name "pubspec.yaml" directory))
    (let ((flutter (with-temp-buffer
                     (insert-file-contents (expand-file-name "pubspec.yaml" directory))
                     (re-search-forward "^[ \t]+sdk:[ \t]+flutter\\(?:[ \t]*#.*\\)?$" nil t))))
      (if flutter '((test . "flutter test") (run . "flutter run"))
        '((test . "dart test") (run . "dart run")))))
   (t (gethash directory +local-project--metadata))))

(defun +local-project--assign (variable value)
  "Respect file/directory local declarations and preserve explicit overrides."
  (let ((owned (assq variable +local-project--assigned)))
    (cond
     ((or (assq variable file-local-variables-alist) (assq variable dir-local-variables-alist)
          (and (local-variable-p variable)
               (or (not owned) (not (equal (symbol-value variable) (cdr owned))))))
      ;; Another package or the user has replaced a buffer-local value.  Give
      ;; up ownership instead of silently reclaiming it on the next refresh.
      (setq +local-project--assigned (assq-delete-all variable +local-project--assigned)
            +local-project--originals (assq-delete-all variable +local-project--originals)))
     (t
      (unless owned
        (setf (alist-get variable +local-project--originals)
              (list :local (local-variable-p variable)
                    :value (and (boundp variable) (symbol-value variable)))))
      (set (make-local-variable variable) value)
      (setf (alist-get variable +local-project--assigned) value)))))

(defun +local-project--unassign (variable)
  "Restore a vanished default only while this module still owns VARIABLE."
  (when-let ((owned (assq variable +local-project--assigned)))
    (when (and (equal (symbol-value variable) (cdr owned))
               (not (assq variable file-local-variables-alist))
               (not (assq variable dir-local-variables-alist)))
      (let ((original (alist-get variable +local-project--originals)))
        (if (plist-get original :local)
            (set variable (plist-get original :value))
          (kill-local-variable variable))))
    (setq +local-project--assigned (assq-delete-all variable +local-project--assigned)
          +local-project--originals (assq-delete-all variable +local-project--originals))))

(defun +local-project--apply (directory)
  (setq-local +local-project--directory directory)
  (condition-case nil
      (setq-local +local-project--defaults (+local-project--read-defaults directory))
    (error (setq-local +local-project--defaults nil)))
  (if-let ((command (alist-get 'build +local-project--defaults)))
      (+local-project--assign 'compile-command command)
    (+local-project--unassign 'compile-command))
  (+local-project--assign 'compilation-read-command (not (alist-get 'build +local-project--defaults)))
  (+local-project--assign 'projectile-project-compilation-dir directory)
  ;; Empty defaults force the native prompt, rather than Projectile's old
  ;; npm install / pub get defaults.  Explicit project declarations still win.
  (dolist (entry '((projectile-project-compilation-cmd . build)
                   (projectile-project-test-cmd . test) (projectile-project-run-cmd . run)))
    (+local-project--assign (car entry) (or (alist-get (cdr entry) +local-project--defaults) ""))))

(defun +local-project--query-result (program object)
  (let ((names (if (equal program "mise")
                   (cl-loop for task in object
                            unless (alist-get "hide" task nil nil #'equal)
                            collect (alist-get "name" task nil nil #'equal))
                 (cl-loop for (name . recipe) in (alist-get "recipes" object nil nil #'equal)
                          unless (or (alist-get "private" recipe nil nil #'equal)
                                     (cl-some (lambda (parameter)
                                                (and (null (alist-get "default" parameter nil nil #'equal))
                                                     (not (equal (alist-get "kind" parameter nil nil #'equal) "star"))))
                                              (alist-get "parameters" recipe nil nil #'equal)))
                          collect name))))
    (cl-loop for (phase . candidates) in '((build "build") (test "test") (run "dev" "run" "start"))
             for name = (cl-find-if (lambda (name) (member name names)) candidates)
             when name collect (cons phase (+local-project--shell
                                            (if (equal program "mise") (list program "run" name)
                                              (list program "--one" name)))))))

(defun +local-project--fingerprint (directory)
  "Identify declaration files without reading or executing their contents."
  (mapcar (lambda (name)
            (let* ((file (expand-file-name name directory))
                   (attributes (file-attributes file 'string)))
              (list file (and attributes (file-attribute-modification-time attributes))
                    (and attributes (file-attribute-size attributes)))))
          +local-project--markers))

(defun +local-project--declaration-program (directory)
  "Select the nearest component's existing declaration provider."
  (cond ((cl-some (lambda (name) (file-exists-p (expand-file-name name directory)))
                  '("Justfile" "justfile" ".justfile")) "just")
        ((cl-some (lambda (name) (file-exists-p (expand-file-name name directory)))
                  '("mise.toml" ".mise.toml")) "mise")))

(defun +local-project--query-cleanup (entry &optional cancel)
  "Release an owned declaration query's timer, process and output buffers."
  (when (listp entry)
    (when cancel (setf (plist-get entry :status) 'cancelled))
    (when-let ((timer (plist-get entry :timer)))
      (cancel-timer timer) (setf (plist-get entry :timer) nil))
    (when-let ((process (plist-get entry :process)))
      (when (and cancel (process-live-p process)) (delete-process process)))
    (dolist (key '(:output :stderr))
      (when-let ((buffer (plist-get entry key)))
        (when (buffer-live-p buffer) (kill-buffer buffer))
        (setf (plist-get entry key) nil)))))

(defun +local-project--invalidate (directory)
  "Expire a component before cancelling its obsolete declaration queries."
  (setq directory (file-name-as-directory (expand-file-name directory)))
  (let (old)
    (maphash (lambda (key entry)
               (when (equal (car key) directory) (push (cons key entry) old)))
             +local-project--queries)
    (dolist (item old)
      (remhash (car item) +local-project--queries)
      (+local-project--query-cleanup (cdr item) t)))
  (remhash directory +local-project--metadata))

(defun +local-project--queries-reset ()
  "Cancel declaration probes on reload; no cached callback may outlive its token."
  (let ((old (hash-table-values +local-project--queries)))
    (clrhash +local-project--queries)
    (clrhash +local-project--metadata)
    (dolist (entry old) (+local-project--query-cleanup entry t)))
  ;; Before tokens were introduced, queries had no process handle in the cache.
  ;; Limit migration cleanup to this provider's old private output buffer name.
  (dolist (process (process-list))
    (let ((buffer (process-buffer process)))
      (when (and (string-prefix-p "project-declarations" (process-name process))
                 (buffer-live-p buffer)
                 (string-prefix-p " *project declarations*" (buffer-name buffer)))
        (when (process-live-p process) (delete-process process))
        (when (buffer-live-p buffer) (kill-buffer buffer))))))

(defun +local-project--query-current-p (key entry)
  "Accept output only for the latest token, declarations and environment."
  (and (eq entry (gethash key +local-project--queries))
       (eq (plist-get entry :status) 'pending)
       (equal (cdr key) (+local-project--declaration-program (car key)))
       (equal (plist-get entry :fingerprint) (+local-project--fingerprint (car key)))
       (or (not (+local-project--environment-p))
           (let ((current (+local-env-context (car key))))
             (and (memq (plist-get current :status) '(ready unmanaged))
                  (equal (plist-get entry :generation) (plist-get current :generation)))))))

(defun +local-project--query (directory program context)
  "Read existing task declarations asynchronously; latest metadata owns its token."
  (let* ((directory (file-name-as-directory (expand-file-name directory)))
         (key (cons directory program))
         (default-directory directory)
         (process-environment (or (plist-get context :process-environment) process-environment))
         (exec-path (or (plist-get context :exec-path) exec-path))
         (executable (executable-find program))
         (fingerprint (+local-project--fingerprint directory))
         (generation (plist-get context :generation))
         (old (gethash key +local-project--queries)))
    (when (and old (or (not (listp old))
                       (not (equal fingerprint (plist-get old :fingerprint)))
                       (not (equal generation (plist-get old :generation)))
                       (not (equal executable (plist-get old :executable)))))
      (remhash key +local-project--queries)
      (+local-project--query-cleanup old t)
      (remhash directory +local-project--metadata)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (equal +local-project--directory directory) (+local-project--apply directory))))
      (setq old nil))
    (when (and executable (not old))
      (let* ((output (generate-new-buffer " *project declarations*"))
             (stderr (generate-new-buffer " *project declaration errors*"))
             (entry (list :token (gensym "declarations-") :status 'pending
                          :fingerprint fingerprint :generation generation :executable executable
                          ;; Predeclare mutable fields: `setf' may otherwise
                          ;; replace the plist head and detach the cached token.
                          :output output :stderr stderr :process nil :timer nil)))
        (puthash key entry +local-project--queries)
        (condition-case nil
            (progn
              (setf (plist-get entry :process)
                    (make-process
                     :name "project-declarations" :buffer output :stderr stderr
                     :connection-type 'pipe :coding 'utf-8-unix :noquery t
                     :command (cons executable (if (equal program "mise")
                                                   '("tasks" "ls" "--json" "--local")
                                                 '("--dump" "--dump-format" "json")))
                     :sentinel
                     (lambda (process _event)
                       (when (memq (process-status process) '(exit signal))
                         (unwind-protect
                             (when (+local-project--query-current-p key entry)
                               (condition-case nil
                                   (progn
                                     (unless (zerop (process-exit-status process)) (error "Declaration probe failed"))
                                     (let ((commands
                                            (with-current-buffer output
                                              (goto-char (point-min))
                                              (let ((json-object-type 'alist) (json-key-type 'string)
                                                    (json-array-type 'list) (json-false nil) (json-null nil))
                                                (+local-project--query-result program (json-read))))))
                                       (setf (plist-get entry :status) 'ready)
                                       (puthash directory commands +local-project--metadata)
                                       (dolist (buffer (buffer-list))
                                         (with-current-buffer buffer
                                           (when (equal +local-project--directory directory)
                                             (+local-project--apply directory))))))
                                 (error (setf (plist-get entry :status) 'failed))))
                           (+local-project--query-cleanup entry))))))
              (setf (plist-get entry :timer)
                    (run-at-time 8 nil
                                 (lambda ()
                                   (when (eq entry (gethash key +local-project--queries))
                                     (setf (plist-get entry :status) 'failed)
                                     (+local-project--query-cleanup entry t))))))
          (error
           (setf (plist-get entry :status) 'failed)
           (+local-project--query-cleanup entry t)))))))

;; This file is reloaded independently by Doom; retire every previous ticket.
(+local-project--queries-reset)

(defun +local-project-prepare-h ()
  (when (and buffer-file-name (not (file-remote-p buffer-file-name)))
    (let ((directory (+local-project--directory)))
      (+local-project--apply directory)
      (unless (cl-some (lambda (name) (file-exists-p (expand-file-name name directory)))
                       '("package.json" "Cargo.toml" "pubspec.yaml"))
        (let ((program (+local-project--declaration-program directory)))
          (when program
            (if (+local-project--environment-p)
                (+local-env-ensure directory
                                   (lambda (context)
                                     (when (memq (plist-get context :status) '(ready unmanaged))
                                       (+local-project--query directory program context))))
              (+local-project--query directory program nil))))))))

(defun +local-project-env-changed-h (old new)
  (unless (equal (plist-get old :generation) (plist-get new :generation))
    (+local-project--invalidate (or +local-project--directory (+local-project--directory))))
  (+local-project-prepare-h))

(defun +local-project-manifest-saved-h ()
  (when (and buffer-file-name
             (member (file-name-nondirectory buffer-file-name) +local-project--markers))
    (let ((directory (file-name-directory buffer-file-name)) affected)
      ;; Workspace declarations can change native defaults in member buffers.
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (and +local-project--directory
                     (or (equal +local-project--directory directory)
                         (file-in-directory-p +local-project--directory directory)))
            (cl-pushnew +local-project--directory affected :test #'equal))))
      (dolist (component affected) (+local-project--invalidate component))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (member +local-project--directory affected) (+local-project-prepare-h)))))))
