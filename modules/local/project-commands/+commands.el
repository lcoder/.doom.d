;;; local/project-commands/+commands.el -*- lexical-binding: t; -*-
(require 'compile)
(defvar projectile-project-root)
(defvar projectile-project-compilation-dir)
(defvar projectile-per-project-compilation-buffer)

(defun +local-project--buffer-name (mode)
  (format "*%s:%s:%s*" (downcase mode)
          (file-name-nondirectory (directory-file-name default-directory))
          (substring (secure-hash 'sha1 (file-truename default-directory)) 0 8)))

(defun +local-project--invoke (directory phase function args)
  "Use native command/history/process handling within the component context."
  (let ((default-directory directory)
        (projectile-project-root directory)
        (projectile-project-compilation-dir directory)
        (projectile-per-project-compilation-buffer nil)
        (compilation-directory directory)
        (compilation-buffer-name-function #'+local-project--buffer-name)
        (compilation-read-command (if phase
                                      (not (alist-get phase +local-project--defaults))
                                    compilation-read-command))
        (+local-project--running t))
    (apply function args)))

(defun +local-project--call (phase function args &optional retry-declarations)
  "Call native FUNCTION with ARGS in PHASE's component environment.
RETRY-DECLARATIONS lets an interactive entry retry expired discovery failures."
  (if (or +local-project--running (file-remote-p default-directory))
      (apply function args)
    (let* ((directory (+local-project--directory))
           (source (current-buffer))
           (context (and (+local-project--environment-p) (+local-env-context directory))))
      (+local-project--apply directory)
      (cond
       ((not context)
        (when retry-declarations (+local-project--retry-declarations directory nil))
        (+local-project--invoke directory phase function args))
       ((memq (plist-get context :status) '(ready unmanaged remote))
        (when retry-declarations (+local-project--retry-declarations directory context))
        (+local-env-call-with-context context #'+local-project--invoke directory phase function args))
       ((eq (plist-get context :status) 'pending)
        (message "正在准备当前组件环境，完成后继续原命令")
        (+local-env-ensure
         directory
         (lambda (ready)
           (when (buffer-live-p source)
             (with-current-buffer source
               (when (equal directory (+local-project--directory))
                 (if (memq (plist-get ready :status) '(ready unmanaged))
                     (progn
                       (when retry-declarations (+local-project--retry-declarations directory ready))
                       (+local-env-call-with-context ready #'+local-project--invoke directory phase function args))
                   (message "命令未运行：%s" (or (plist-get ready :reason) "项目环境不可用")))))))))
       (t (user-error "项目环境不可用：%s" (or (plist-get context :reason) "请补齐项目声明的工具")))))))

(defun +local-project-compile-a (function &rest args)
  "Adapt an interactive compile without changing internal callers' contract.
Doom and other packages expect noninteractive `compile' to return its
compilation buffer immediately.  Only a user command may wait for the
asynchronous environment preparation.  Projectile already binds the context
around its native command with `+local-project--running'."
  (if (called-interactively-p 'interactive)
      (+local-project--call nil function args t)
    (apply function args)))
(defun +local-project-build-a (function &rest args)
  (+local-project--call 'build function args (called-interactively-p 'interactive)))
(defun +local-project-test-a (function &rest args)
  (+local-project--call 'test function args (called-interactively-p 'interactive)))
(defun +local-project-run-a (function &rest args)
  (+local-project--call 'run function args (called-interactively-p 'interactive)))
(defun +local-project-repeat-a (function &rest args)
  (+local-project--call nil function args (called-interactively-p 'interactive)))

(defun +local-project-recompile-a (function &rest args)
  "Find the current component's output only for an interactive recompile."
  (cond
   ((not (called-interactively-p 'interactive)) (apply function args))
   ((or +local-project--running (derived-mode-p 'compilation-mode 'comint-mode))
    (+local-project--call nil function args t))
   (t
    (let* ((directory (+local-project--directory))
           (buffer (cl-find-if
                    (lambda (candidate)
                      (with-current-buffer candidate
                        (and (derived-mode-p 'compilation-mode)
                             compilation-arguments
                             (equal (file-name-as-directory (expand-file-name default-directory)) directory))))
                    (buffer-list))))
      (if buffer
          (with-current-buffer buffer (+local-project--call nil function args t))
        (+local-project--call nil function args t))))))
