;;; local/languages/debug-query.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(defvar +local-languages--debug-cache (make-hash-table :test #'equal)
  "In-memory results of read-only native debugger discovery.")
(defvar-local +local-languages--debug-ticket 0)

(defun +local-languages--executable (name)
  "Use optional public machine overrides while keeping native discovery when disabled."
  (if (and +local-languages--env-enabled (fboundp '+local-env-executable-find))
      (+local-env-executable-find name)
    (executable-find name)))

(defun +local-languages--debug-key (kind directory context)
  "Identify cached KIND discovery by environment and relevant project files."
  (list kind directory (+local-languages--environment-key context)
        (plist-get context :generation)
        (when (eq kind 'cargo)
          (mapcar (lambda (name)
                    (list name (ignore-errors (file-attributes (expand-file-name name directory)))))
                  '("Cargo.toml" "Cargo.lock" "rust-toolchain" "rust-toolchain.toml")))))

(defun +local-languages--query (context program args callback)
  "Run a read-only PROGRAM/ARGS query asynchronously, with bounded lifetime."
  (+local-languages--call
   context
   (lambda ()
     (let* ((output (generate-new-buffer " *native debug query*"))
            (errors (generate-new-buffer " *native debug query errors*"))
            (process (make-process :name "native-debug-discovery" :buffer output :stderr errors
                                   :command (cons (or (+local-languages--executable program) program) args)
                                   :connection-type 'pipe :noquery t
                                   :sentinel #'ignore))
            (timer (run-with-timer 8 nil (lambda ()
                                          (when (process-live-p process) (delete-process process))))))
       (set-process-sentinel
        process
        (lambda (process _event)
          (when (memq (process-status process) '(exit signal))
            (cancel-timer timer)
            (unwind-protect
                (funcall callback (and (eq (process-status process) 'exit)
                                       (zerop (process-exit-status process))
                                       (with-current-buffer output (buffer-string))))
              (when (buffer-live-p output) (kill-buffer output))
              (when (buffer-live-p errors) (kill-buffer errors))))))
       process))))

(defun +local-languages--debug-cache-finish (key value)
  "Publish a read-only VALUE and wake native debugger requests awaiting KEY."
  (let ((job (gethash key +local-languages--debug-cache)))
    (puthash key (list :state (if value 'ready 'failed) :value value :started (float-time))
             +local-languages--debug-cache)
    (dolist (callback (plist-get job :callbacks)) (funcall callback value))))

(defun +local-languages--discover-adapter (context callback)
  "Discover installed adapters without any synchronous external commands."
  (+local-languages--call
   context
   (lambda ()
     (cl-labels
         ((fallback ()
            (funcall callback
                     (+local-languages--call context
                                            (lambda ()
                                              (when-let ((path (+local-languages--executable "codelldb")))
                                                (cons path "lldb"))))))
          (brew-query ()
            (if-let ((brew (+local-languages--executable "brew")))
                (+local-languages--query
                 context brew '("--prefix" "llvm")
                 (lambda (result)
                   (let* ((prefix (and result (string-trim result)))
                          (path (and prefix (file-name-absolute-p prefix)
                                     (expand-file-name "bin/lldb-dap" prefix))))
                     (if (and path (file-executable-p path))
                         (funcall callback (cons path "lldb-dap"))
                       (fallback)))))
              (fallback))))
       (cond ((+local-languages--executable "lldb-dap")
              (funcall callback (cons (+local-languages--executable "lldb-dap") "lldb-dap")))
             ((+local-languages--executable "xcrun")
              (+local-languages--query
               context (+local-languages--executable "xcrun") '("--find" "lldb-dap")
               (lambda (result)
                 (let ((path (and result (string-trim result))))
                   (if (and path (file-executable-p path))
                       (funcall callback (cons path "lldb-dap"))
                     (+local-languages--call context #'brew-query))))))
             (t (brew-query)))))))

(defun +local-languages--debug-discover (kind directory context callback)
  "Coalesce native debugger KIND preparation and CALLBACK in DIRECTORY."
  (let* ((key (+local-languages--debug-key kind directory context))
         (job (gethash key +local-languages--debug-cache)))
    (cond ((eq (plist-get job :state) 'ready) (funcall callback (plist-get job :value)))
          ((eq (plist-get job :state) 'pending)
           (puthash key (plist-put job :callbacks (cons callback (plist-get job :callbacks)))
                    +local-languages--debug-cache))
          ((and (eq (plist-get job :state) 'failed)
                (< (- (float-time) (plist-get job :started)) 300)) (funcall callback nil))
          (t
           (puthash key (list :state 'pending :callbacks (list callback)) +local-languages--debug-cache)
           (condition-case nil
               (if (eq kind 'cargo)
                   (+local-languages--query
                    context "cargo" '("metadata" "--format-version" "1" "--no-deps" "--offline" "--locked")
                    (lambda (output)
                      (+local-languages--debug-cache-finish
                       key (and output (ignore-errors (+local-languages--json output))))))
                 (+local-languages--discover-adapter
                  context (apply-partially #'+local-languages--debug-cache-finish key)))
             (error (+local-languages--debug-cache-finish key nil)))))))

(defun +local-languages--debug-value (kind directory)
  "Read a prepared discovery value without starting a subprocess."
  (plist-get (gethash (+local-languages--debug-key kind directory
                                                (+local-languages--context directory))
                     +local-languages--debug-cache) :value))

(defun +local-languages--dape-ready (function args directory)
  "Resume the native Dape entry after its read-only discovery has completed."
  (let* ((source (current-buffer))
         (context (+local-languages--context directory))
         (ticket (cl-incf +local-languages--debug-ticket))
         (configuration (car args))
         (remaining (if (plist-get configuration :program) 1 2))
         failed)
    (cl-labels ((done (value)
                  (unless value (setq failed t))
                  (setq remaining (1- remaining))
                  (when (and (zerop remaining) (buffer-live-p source))
                    (with-current-buffer source
                      (when (and (= ticket +local-languages--debug-ticket)
                                 (equal (plist-get context :generation)
                                        (plist-get (+local-languages--context directory) :generation)))
                        (if failed
                            (message "Rust debug discovery is unavailable; basic editing remains available")
                          (+local-languages--call context (lambda () (apply function args)))))))))
      (+local-languages--debug-discover 'adapter directory context #'done)
      (unless (plist-get configuration :program)
        (+local-languages--debug-discover 'cargo directory context #'done)))))

(provide '+local-languages-debug-query)
