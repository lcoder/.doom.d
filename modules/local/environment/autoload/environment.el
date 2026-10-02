;;; local/environment/autoload/environment.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +local-env-context (&optional directory)
  "Return DIRECTORY's cached environment, without I/O or starting a process.
The result has :directory, :status, :process-environment, :exec-path,
:generation and :reason.  Status is pending, ready, unmanaged, unavailable,
or remote.  Unknown local directories are pending; call `+local-env-ensure'
to prepare them.  Environment values are private, in-memory data."
  (let* ((directory (file-name-as-directory
                     (expand-file-name (or directory default-directory))))
         (record (gethash (gethash directory +local-env--directories)
                          +local-env--cache))
         (context (plist-get record :context)))
    (if (file-remote-p directory)
        (list :directory directory :status 'remote :generation 0 :reason nil
              :process-environment (copy-sequence process-environment)
              :exec-path (copy-sequence exec-path))
      (list :directory directory :status (or (plist-get context :status) 'pending)
            :generation (or (plist-get context :generation) 0)
            :reason (plist-get context :reason)
            :process-environment
            (copy-sequence (or (plist-get context :process-environment)
                               +local-env--base-environment
                               (default-value 'process-environment)))
            :exec-path (copy-sequence (or (plist-get context :exec-path)
                                         +local-env--base-exec-path
                                         (default-value 'exec-path)))))))

;;;###autoload
(defun +local-env-ensure (&optional directory callback force)
  "Prepare DIRECTORY asynchronously; call CALLBACK with its settled context.
Coalesce equivalent in-flight requests.  FORCE invalidates the current request.
CALLBACK runs in its originating buffer when that buffer is still live.  File
buffers receive local environments and `+local-env-changed-hook' notifications.
An explicitly requested other directory never changes the editing buffer."
  (+local-env--ensure (or directory default-directory) callback force))

;;;###autoload
(defun +local-env-call-with-context (context function &rest args)
  "Call FUNCTION with ARGS under CONTEXT's directory and environment."
  (unless (memq (plist-get context :status) '(ready unmanaged remote))
    (user-error "项目环境尚未就绪，请稍后重试"))
  (let ((default-directory (plist-get context :directory))
        (process-environment (copy-sequence (plist-get context :process-environment)))
        (exec-path (copy-sequence (plist-get context :exec-path))))
    (apply function args)))

;;;###autoload
(defun +local-env-executable-find (name)
  "Find NAME by machine overrides, then the current context's `exec-path'."
  (let ((override (or (alist-get name +local-env-tool-overrides nil nil #'equal)
                      (alist-get (intern name) +local-env-tool-overrides)
                      (alist-get name my/dev-tool-overrides nil nil #'equal)
                      (alist-get (intern name) my/dev-tool-overrides))))
    (if override
        (let ((path (expand-file-name override)))
          (and (file-executable-p path) (not (file-directory-p path)) path))
      (executable-find name))))

;;;###autoload
(defun +local-env-refresh ()
  "Asynchronously refresh environments of active file buffers."
  (interactive)
  (+local-env--capture-base)
  (+local-env--recheck t))

