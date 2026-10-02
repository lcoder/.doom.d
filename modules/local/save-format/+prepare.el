;;; +prepare.el --- Nonblocking formatter prerequisites -*- lexical-binding: t; -*-

(defvar +local-save-format--jobs (make-hash-table :test #'equal))
(defvar-local +local-save-format--rust-metadata nil)
(defvar-local +local-save-format--dart-stdin-supported nil)
(defvar-local +local-save-format--runtime-context nil)
(defvar-local +local-save-format--selection nil)
(defvar-local +local-save-format--request nil)
(defvar-local +local-save-format--last-notice nil)
(defvar +local-save-format--prepared nil)
(defvar +local-save-format--prepared-resources nil)

(defun +local-save-format--env-p ()
  "Whether the optional environment module exposes its public API."
  (and (modulep! :local environment) (fboundp '+local-env-context)))

(defun +local-save-format--context (&optional directory)
  "Query a context without resolving or invoking subprocesses."
  (if (+local-save-format--env-p)
      (+local-env-context directory)
    (list :directory (or directory default-directory) :status 'unmanaged
          :generation 'native :process-environment (copy-sequence process-environment)
          :exec-path (copy-sequence exec-path))))

(defun +local-save-format--call-context (context function &rest args)
  "Call FUNCTION with ARGS under an immutable public CONTEXT snapshot."
  (if (and (+local-save-format--env-p) (fboundp '+local-env-call-with-context))
      (apply #'+local-env-call-with-context context function args)
    (let ((default-directory (or (plist-get context :directory) default-directory))
          (process-environment (or (plist-get context :process-environment) process-environment))
          (exec-path (or (plist-get context :exec-path) exec-path)))
      (apply function args))))

(defun +local-save-format--context-key (context)
  "Return an in-memory cache identity without exposing environment values."
  (list (plist-get context :directory) (plist-get context :generation)
        (plist-get context :exec-path) (plist-get context :process-environment)))

(defun +local-save-format--directories (&optional directory)
  "Return DIRECTORY's ancestors within Git or home, nearest first."
  (let* ((directory (file-name-as-directory (expand-file-name (or directory default-directory))))
         (home (file-name-as-directory (expand-file-name "~")))
         (boundary (or (locate-dominating-file directory ".git")
                       (and (file-in-directory-p directory home) home) directory))
         result parent)
    (if (file-remote-p directory) (list directory)
      (while directory
        (push directory result)
        (setq parent (file-name-directory (directory-file-name directory))
              directory (unless (or (equal directory boundary) (equal directory parent)) parent)))
      (nreverse result))))

(defun +local-save-format--notice (text)
  "Show formatter TEXT once until this buffer's state changes."
  (unless (equal text +local-save-format--last-notice)
    (setq +local-save-format--last-notice text)
    (message "项目格式化：%s" text)))

(defun +local-save-format--job-finish (key entry status value)
  "Finish cached ENTRY once and notify its waiting buffers."
  (when (eq (plist-get entry :status) 'pending)
    (setf (plist-get entry :status) status (plist-get entry :value) value)
    (setf (plist-get entry :finished-at) (float-time))
    (when-let ((timer (plist-get entry :timer))) (cancel-timer timer))
    (dolist (buffer (list (plist-get entry :output) (plist-get entry :stderr)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))
    (let ((callbacks (nreverse (plist-get entry :callbacks))))
      (setf (plist-get entry :callbacks) nil)
      (puthash key entry +local-save-format--jobs)
      (dolist (callback callbacks)
        (condition-case err (funcall callback entry)
          (error (message "Formatter preparation callback: %s" (error-message-string err))))))))

(defun +local-save-format--job (key context directory program args parser callback)
  "Share a bounded background prerequisite and invoke CALLBACK with its entry.
PARSER receives its stdout buffer and exit status.  Output is never printed."
  (when-let ((entry (gethash key +local-save-format--jobs)))
    ;; Retry transient failures on a later save, without a tight polling loop.
    (when (and (eq (plist-get entry :status) 'unavailable)
               (> (- (float-time) (or (plist-get entry :finished-at) 0)) 15))
      (remhash key +local-save-format--jobs)))
  (if-let ((entry (gethash key +local-save-format--jobs)))
      (if (eq (plist-get entry :status) 'pending)
          (push callback (plist-get entry :callbacks))
        (funcall callback entry))
    (let* ((output (generate-new-buffer " *local-format-prepare*"))
           (stderr (generate-new-buffer " *local-format-prepare-error*"))
           (entry (list :status 'pending :callbacks (list callback)
                        :output output :stderr stderr)))
      (puthash key entry +local-save-format--jobs)
      (condition-case nil
          (+local-save-format--call-context
           context
           (lambda ()
             (let ((default-directory directory)
                   (executable (executable-find program)))
               (unless executable (error "Missing formatter prerequisite"))
               (setf (plist-get entry :process)
                     (make-process
                      :name "local-format-prepare" :buffer output :stderr stderr
                      :command (cons executable args) :connection-type 'pipe
                      :coding 'utf-8-unix :noquery t
                      :sentinel
                      (lambda (process _event)
                        (when (and (not (process-live-p process))
                                   (eq (plist-get entry :status) 'pending))
                          (condition-case nil
                              (+local-save-format--job-finish
                               key entry 'ready (funcall parser output (process-exit-status process)))
                            (error (+local-save-format--job-finish key entry 'unavailable nil)))))))
               (setf (plist-get entry :timer)
                     (run-at-time
                      +local-save-format-prepare-timeout nil
                      (lambda ()
                        (+local-save-format--job-finish key entry 'unavailable nil)
                        (when-let ((process (plist-get entry :process)))
                          (when (process-live-p process) (delete-process process)))))))))
        (error (+local-save-format--job-finish key entry 'unavailable nil))))))

(defun +local-save-format--cargo-files (directory)
  "Watch Cargo workspace declarations, locks and toolchain settings."
  (cl-loop for dir in (+local-save-format--directories directory)
           append (mapcar (lambda (name) (expand-file-name name dir))
                          '("Cargo.toml" "Cargo.lock" "rust-toolchain" "rust-toolchain.toml"
                            ".cargo/config" ".cargo/config.toml"))))

(defun +local-save-format--cargo-prepare (context callback)
  "Obtain offline, locked Cargo metadata asynchronously for this source buffer."
  (let* ((directory (locate-dominating-file (+local-save-format--source-directory) "Cargo.toml"))
         (files (and directory (+local-save-format--cargo-files directory)))
         (executable (+local-save-format--call-context context #'executable-find "cargo"))
         (key (list 'cargo directory (+local-save-format--context-key context)
                    executable (and executable
                                    (+local-save-format--file-signatures
                                     (list executable (file-truename executable))))
                    (and files (+local-save-format--file-signatures files)))))
    (if (not directory) (funcall callback (list :status 'unavailable))
      (+local-save-format--job
       key context directory "cargo"
       '("metadata" "--offline" "--locked" "--no-deps" "--format-version" "1")
       (lambda (output status)
         (unless (zerop status) (error "Cargo metadata unavailable"))
         (with-current-buffer output
           (goto-char (point-min))
           (let ((json-object-type 'alist) (json-array-type 'list) (json-key-type 'string)
                 (json-null nil) (json-false :false))
             (let ((metadata (json-read)))
               (unless (alist-get "workspace_root" metadata nil nil #'equal)
                 (error "Invalid Cargo metadata"))
               metadata))))
       callback))))

(defun +local-save-format--cargo-metadata (_directory)
  "Return already prepared Cargo metadata without synchronous work."
  (or (plist-get +local-save-format--prepared-resources :rust-metadata)
      +local-save-format--rust-metadata (error "Rust 项目元数据尚未就绪")))

(defun +local-save-format--dart-executable ()
  "Find Dart under the prepared project environment."
  (or (executable-find "dart") (error "当前项目中找不到 Dart")))

(defun +local-save-format--dart-prepare (context callback)
  "Probe Dart stdin-name capability in the background, without version assumptions."
  (let* ((executable (+local-save-format--call-context context #'executable-find "dart"))
         (key (list 'dart executable (+local-save-format--context-key context)
                    (and executable
                         (+local-save-format--file-signatures
                          (list executable (file-truename executable)))))))
    (if (not executable) (funcall callback (list :status 'unavailable))
      (+local-save-format--job
       key context (+local-save-format--source-directory) "dart" '("format" "-v" "--help")
       (lambda (output status)
         ;; Older CLIs may reject verbose help; retain their original stdin command.
         (and (zerop status)
              (with-current-buffer output
                (goto-char (point-min)) (not (null (search-forward "--stdin-name" nil t))))))
       callback))))

(defun +local-save-format--dart-stdin-arguments ()
  "Use the real source path only when the prepared SDK supports it."
  (when (and buffer-file-name
             (if +local-save-format--prepared-resources
                 (plist-get +local-save-format--prepared-resources :dart-stdin)
               +local-save-format--dart-stdin-supported))
    (list "--stdin-name" (apheleia-formatters-local-buffer-file-name))))

(defun +local-save-format--prepare (formatters callback)
  "Prepare FORMATTERS without waiting; CALLBACK receives context and success."
  (let ((buffer (current-buffer))
        (context (+local-save-format--context (+local-save-format--source-directory))))
    (pcase (plist-get context :status)
      ('pending
       (if (and (+local-save-format--env-p) (fboundp '+local-env-ensure))
           (+local-env-ensure
            (+local-save-format--source-directory)
            (lambda (_context)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer (+local-save-format--prepare formatters callback)))))
         (funcall callback context nil)))
      ('unavailable
       (if (and (+local-save-format--env-p) (fboundp '+local-env-ensure))
           (+local-env-ensure
            (+local-save-format--source-directory)
            (lambda (new)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (if (memq (plist-get new :status) '(ready unmanaged))
                      (+local-save-format--prepare formatters callback)
                    (funcall callback new nil))))))
         (funcall callback context nil)))
      (_
       (let* ((needed (delq nil (list (and (memq 'rustfmt formatters) 'cargo)
                                      (and (memq 'dart-format formatters) 'dart))))
              (remaining (length needed)) (ok t))
         (if (or (not (+local-save-format--env-p)) (null needed))
             (funcall callback context t)
           (dolist (kind needed)
             (let ((kind kind))
               (funcall
                (if (eq kind 'cargo) #'+local-save-format--cargo-prepare #'+local-save-format--dart-prepare)
                context
                (lambda (entry)
                  (when (buffer-live-p buffer)
                    (with-current-buffer buffer
                      (setq ok (and ok (eq (plist-get entry :status) 'ready)))
                      (if (eq kind 'cargo)
                          (setq +local-save-format--rust-metadata (plist-get entry :value))
                        (setq +local-save-format--dart-stdin-supported (plist-get entry :value)))
                      (when (zerop (cl-decf remaining)) (funcall callback context ok))))))))))))))

(defun +local-save-format--prefetch (&rest _args)
  "Warm format metadata after environment readiness, never on a blocking path."
  (when (and (+local-save-format--env-p) buffer-file-name
             (not (file-remote-p buffer-file-name)) (fboundp 'apheleia--get-formatters))
    (let ((formatters (apheleia--get-formatters)))
      (when formatters (+local-save-format--prepare formatters #'ignore)))))
