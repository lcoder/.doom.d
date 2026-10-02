;;; local/languages/lifecycle.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(defvar +local-languages--env-enabled nil)
(defvar +local-languages--server-jobs (make-hash-table :test #'equal))
(defvar +local-languages--lsp-environments (make-hash-table :test #'eq :weakness 'key))
(defvar +local-languages--retired-workspaces (make-hash-table :test #'eq :weakness 'key))
(defvar +local-languages--resuming nil)
(defvar +local-languages--filtering nil)
(defvar +local-languages--recovery-timer nil
  "A single idle timer that rechecks only postponed language capabilities.")
(defvar-local +local-languages--request nil
  "The latest explicit run or debug request waiting for its environment.")
(defvar-local +local-languages--lsp-request nil
  "Language-service startup, independent of explicit run and debug requests.")
(defvar-local +local-languages--request-id 0)
(put '+local-languages--request 'permanent-local t)
(put '+local-languages--lsp-request 'permanent-local t)
(put '+local-languages--request-id 'permanent-local t)
(declare-function +local-env-context nil (&optional directory))
(declare-function +local-env-ensure nil (&optional directory callback force))
(declare-function +local-env-call-with-context nil (context function &rest args))

(defun +local-languages--context (&optional directory)
  "Query only the env module's public contract, or use the native environment."
  (if (and +local-languages--env-enabled (fboundp '+local-env-context))
      (+local-env-context directory)
    (list :directory (or directory default-directory) :status 'unmanaged :generation 0
          :process-environment (copy-sequence process-environment)
          :exec-path (copy-sequence exec-path))))

(defun +local-languages--call (context function &rest args)
  "Call FUNCTION with ARGS in CONTEXT without exposing environment values."
  (if (and +local-languages--env-enabled (fboundp '+local-env-call-with-context))
      (apply #'+local-env-call-with-context context function args)
    (let ((process-environment (copy-sequence (plist-get context :process-environment)))
          (exec-path (copy-sequence (plist-get context :exec-path))))
      (apply function args))))

(defun +local-languages--ready-p (context)
  "Whether CONTEXT permits native language-service startup."
  (memq (plist-get context :status) '(ready unmanaged remote)))

(defun +local-languages--environment-key (&optional context)
  "Return an opaque identity for CONTEXT's tool environment."
  (let ((context (or context (+local-languages--context))))
    (secure-hash 'sha256
                 (prin1-to-string (list (plist-get context :process-environment)
                                        (plist-get context :exec-path))))))

(defun +local-languages--request-current-p (buffer request context)
  "Check that BUFFER still owns REQUEST and CONTEXT is not stale."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (and (eq request (if (plist-get request :lsp)
                              +local-languages--lsp-request
                            +local-languages--request))
              (equal (plist-get context :generation)
                     (plist-get (+local-languages--context (plist-get request :directory)) :generation))))))

(defun +local-languages--resume (buffer request context)
  "Resume REQUEST in BUFFER after asynchronous environment preparation."
  (when (and (+local-languages--request-current-p buffer request context)
             (+local-languages--ready-p context))
    (with-current-buffer buffer
      (+local-languages--call
       context
       (lambda ()
         (if (and (eq (plist-get request :lsp) t) (not (file-remote-p default-directory))
                  (+local-languages--prepare-server context request))
             nil
           (let ((+local-languages--resuming t)
                 (lsp-enable-suggest-server-download nil))
             (if (plist-get request :lsp)
                 (setq +local-languages--lsp-request nil)
               (setq +local-languages--request nil))
             (apply (plist-get request :function) (plist-get request :args)))))))))

(defun +local-languages--guard (function args lsp &optional directory)
  "Defer a native FUNCTION with ARGS until its environment is prepared."
  (if +local-languages--resuming
      (apply function args)
    (let* ((buffer (current-buffer))
           (directory (or directory default-directory))
           (request (list :id (cl-incf +local-languages--request-id)
                          :function function :args args :lsp lsp :directory directory))
           (context (+local-languages--context directory)))
      (if lsp
          (setq +local-languages--lsp-request request)
        (setq +local-languages--request request))
      (if (+local-languages--ready-p context)
          (+local-languages--resume buffer request context)
        (when (and +local-languages--env-enabled (fboundp '+local-env-ensure))
          (+local-env-ensure
           directory
           (apply-partially #'+local-languages--resume buffer request)))))))

(defun +local-languages--guard-lsp (function &rest args)
  "Preserve the original lsp/lsp-deferred entry points and their arguments."
  (+local-languages--guard function args t))

(defun +local-languages--guard-eglot (function &rest args)
  "Keep Eglot startup inside the selected native project environment."
  (+local-languages--guard function args 'eglot))

(defun +local-languages--server-finished (key success)
  "Resume native requests waiting for the LSP client represented by KEY."
  (let ((job (gethash key +local-languages--server-jobs)))
    (puthash key (plist-put job :state (if success 'ready 'failed))
             +local-languages--server-jobs)
    (if success
        (dolist (entry (plist-get job :waiters))
          (when (buffer-live-p (car entry))
            (with-current-buffer (car entry)
              (+local-languages--resume
               (car entry) (cdr entry)
               (+local-languages--context (plist-get (cdr entry) :directory))))))
      (unless (plist-get job :notified)
        (puthash key (plist-put (gethash key +local-languages--server-jobs) :notified t)
                 +local-languages--server-jobs)
        (message "Language server %s could not be prepared; basic editing remains available" (car key))))))

(defun +local-languages--prepare-server (context request)
  "Use a native asynchronous LSP installer when an eligible server is missing.
Return non-nil while startup should stay deferred.  Existing servers and
clients without native installers keep their original behavior."
  (when (cl-every #'fboundp '(lsp--require-packages lsp--filter-clients lsp--supports-buffer?
                              lsp--server-binary-present? lsp--client-download-server-fn
                              lsp--client-server-id lsp--client-priority))
    (lsp--require-packages)
    (let* ((clients (lsp--filter-clients #'lsp--supports-buffer?))
           (installed (cl-some #'lsp--server-binary-present? clients))
           (downloadable (cl-remove-if-not #'lsp--client-download-server-fn clients)))
      (when (and (not installed) downloadable)
        (let* ((client (car (sort downloadable
                                  (lambda (left right)
                                    (> (lsp--client-priority left) (lsp--client-priority right))))))
               (key (list (lsp--client-server-id client)
                          (+local-languages--environment-key context)
                          (executable-find "node") (executable-find "npm")
                          (ignore-errors (network-interface-list))))
               (job (gethash key +local-languages--server-jobs))
               (notified (plist-get job :notified))
               (waiter (cons (current-buffer) request)))
          (when (and job (eq (plist-get job :state) 'failed)
                     (> (- (float-time) (or (plist-get job :started) 0)) 300))
            (remhash key +local-languages--server-jobs)
            (setq job nil))
          (if job
              (puthash key (plist-put job :waiters
                                      (cons waiter (cl-remove (current-buffer)
                                                              (plist-get job :waiters) :key #'car)))
                       +local-languages--server-jobs)
            (puthash key (list :state 'pending :started (float-time) :notified notified :waiters (list waiter))
                     +local-languages--server-jobs)
            (condition-case nil
                (funcall (lsp--client-download-server-fn client) client
                         (lambda (&rest _) (+local-languages--server-finished key t))
                         (lambda (&rest _) (+local-languages--server-finished key nil)) nil)
              (error (+local-languages--server-finished key nil))))
          t)))))

(defun +local-languages--remember-workspace (workspace)
  "Record which environment created a native LSP WORKSPACE."
  (when workspace
    (puthash workspace (+local-languages--environment-key)
             +local-languages--lsp-environments))
  workspace)

(defun +local-languages--compatible-workspace-p (workspace)
  "Keep servers from different tool environments from being shared."
  (or (file-remote-p default-directory)
      (equal (gethash workspace +local-languages--lsp-environments)
             (+local-languages--environment-key))))

(defun +local-languages--filter-workspaces (workspaces)
  "Filter native workspace discovery only inside a compatibility lookup."
  (if +local-languages--filtering
      (cl-remove-if-not #'+local-languages--compatible-workspace-p workspaces)
    workspaces))

(defun +local-languages--find-workspace (function session client root)
  "Limit native FUNCTION's lookup to compatible SESSION/CLIENT/ROOT servers."
  (let* ((table (lsp-session-folder->servers session))
         (workspaces (gethash root table)))
    (unwind-protect
        (progn
          (puthash root (cl-remove-if-not #'+local-languages--compatible-workspace-p workspaces) table)
          (funcall function session client root))
      (if workspaces (puthash root workspaces table) (remhash root table)))))

(defun +local-languages--find-multiroot (function &rest args)
  "Use the native multiroot search after excluding incompatible workspaces."
  (let ((+local-languages--filtering t)) (apply function args)))

(defun +local-languages--environment-changed (old new)
  "Retire only affected servers, then reconnect each buffer in its own context."
  (when (and old
             (or (not (+local-languages--ready-p new))
                 (not (equal (+local-languages--environment-key old)
                             (+local-languages--environment-key new)))))
    (when (and (bound-and-true-p lsp-mode) (fboundp 'lsp-workspaces))
      (dolist (workspace (lsp-workspaces))
        (unless (gethash workspace +local-languages--retired-workspaces)
          (puthash workspace t +local-languages--retired-workspaces)
          (let ((buffers (copy-sequence (lsp--workspace-buffers workspace))))
            (lsp-workspace-shutdown workspace)
            (dolist (buffer buffers)
              (when (buffer-live-p buffer)
                (with-current-buffer buffer
                  (when (fboundp 'lsp-deferred) (lsp-deferred)))))))))
    (when (and (bound-and-true-p eglot--managed-mode) (fboundp 'eglot-current-server))
      (when-let ((server (eglot-current-server)))
        (if (+local-languages--ready-p new)
            (+local-languages--call new #'eglot-reconnect server)
          (eglot-shutdown server)))))
  (when (+local-languages--ready-p new)
    (+local-languages--mode-ready)))

(defun +local-languages--mode-ready ()
  "Restore postponed language and explicit requests independently."
  (dolist (request (list +local-languages--lsp-request +local-languages--request))
    (when request
      (+local-languages--resume
       (current-buffer) request (+local-languages--context (plist-get request :directory))))))

(defun +local-languages--recover-postponed ()
  "Recheck postponed capabilities without making the user reopen any files."
  (maphash
   (lambda (language job)
     (when (eq (plist-get job :state) 'failed)
       (dolist (buffer (plist-get job :buffers))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (+local-languages--prepare-grammar language buffer)
             (+local-languages--upgrade buffer))))))
   +local-languages--grammar-jobs)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (+local-languages--mode-ready))))

(defun +local-languages--start-recovery ()
  "Install one bounded, idle-only recovery check, safely across Doom reloads."
  (when (timerp +local-languages--recovery-timer)
    (cancel-timer +local-languages--recovery-timer))
  (setq +local-languages--recovery-timer
        (run-with-idle-timer 5 t #'+local-languages--recover-postponed)))

(provide '+local-languages-lifecycle)
