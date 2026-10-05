;;; local/environment/autoload/environment.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +local-env-context (&optional directory)
  "Return DIRECTORY's cached environment, without I/O or starting a process.
The result has :directory, :status, :process-environment, :exec-path, :tools,
:generation and :reason.  Tools describe selected mise versions and sources.
Status is pending, ready, unmanaged, unavailable,
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
            :tools (copy-tree (plist-get context :tools))
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
(defun +local-env-project-tool (name &optional directory)
  "Return cached mise tool NAME declared within DIRECTORY's project.
Global defaults do not constitute a project declaration.  No CLI is invoked."
  (let* ((directory (+local-env--directory (or directory default-directory)))
         (boundary (or (locate-dominating-file directory ".git")
                       (file-name-as-directory (expand-file-name "~"))))
         (tool (cl-find name (plist-get (+local-env-context directory) :tools)
                        :key (lambda (entry) (plist-get entry :name)) :test #'equal))
         (source (plist-get tool :source)))
    (when source
      (catch 'found
        (dolist (ancestor (+local-env--ancestors directory))
          ;; The home-level .config/mise directory contains personal defaults.
          (when (equal ancestor (file-name-as-directory (expand-file-name "~")))
            (throw 'found nil))
          (when (or (equal (file-name-directory source) (file-truename ancestor))
                    (cl-some (lambda (relative)
                               (file-in-directory-p source (expand-file-name relative ancestor)))
                             '(".mise/" ".config/mise/" "mise/")))
            (throw 'found (plist-put (copy-tree tool) :directory ancestor)))
          (when (equal ancestor boundary) (throw 'found nil)))))))

;;;###autoload
(defun +local-env-project-tool-executable (tool name &optional directory)
  "Find NAME only inside the selected, installed project mise TOOL.
The cached environment must be ready; unrelated executables on PATH are ignored."
  (let* ((context (+local-env-context directory))
         (entry (+local-env-project-tool tool directory))
         (installation (plist-get entry :install-directory)))
    (when (and (eq (plist-get context :status) 'ready)
               (plist-get entry :installed) installation)
      (when-let ((executable (+local-env-call-with-context context #'executable-find name)))
        (when (file-in-directory-p (file-truename executable) (file-truename installation))
          executable)))))

;;;###autoload
(defun +local-env-refresh ()
  "Asynchronously refresh environments of active file buffers."
  (interactive)
  (+local-env--capture-base)
  (+local-env--recheck t))

