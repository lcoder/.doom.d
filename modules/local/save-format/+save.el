;;; +save.el --- Conservative saving and native Apheleia integration -*- lexical-binding: t; -*-

(defvar apheleia-formatters)
(defvar apheleia-formatter)
(defvar apheleia-mode)
(defvar super-save-in-progress nil)
(defvar +local-save-format--native-formatters nil)
(defvar +local-save-format--prepared-selection nil)

(defun +local-save-format--inhibited-p ()
  "Check existing formatter overrides without consulting environment subprocesses."
  (or (null my/dev-format-choice) (bound-and-true-p apheleia-inhibit)
      (bound-and-true-p +format-inhibit) (bound-and-true-p super-save-in-progress)))

(defun +local-save-format--resolve (fallback)
  "Choose a project formatter; explicit Apheleia/format settings take precedence."
  (cond
   ((+local-save-format--inhibited-p) nil)
   ((not (eq my/dev-format-choice 'auto))
    (list :formatters (if (listp my/dev-format-choice) my/dev-format-choice
                        (list my/dev-format-choice))))
   ((bound-and-true-p apheleia-formatter) (list :formatters fallback))
   (t (or (run-hook-with-args-until-success '+local-save-format-resolvers)
          (list :formatters fallback :language-default t)))))

(defun +local-save-format--get-formatters-a (original &optional interactive)
  "Select formatters without environment resolution or CLI probes."
  (cond
   (+local-save-format--prepared-selection
    (plist-get +local-save-format--prepared-selection :formatters))
   ((or (not (+local-save-format--env-p))
        (file-remote-p (or buffer-file-name default-directory)))
    (funcall original interactive))
   (t
    (condition-case err
        (let ((selection (+local-save-format--resolve (funcall original nil))))
          (setq +local-save-format--selection selection)
          (if (plist-get selection :blocked)
              (progn (+local-save-format--notice (plist-get selection :reason)) nil)
            (let ((formatters (plist-get selection :formatters)))
              (if (and (or (eq interactive 'prompt) (and interactive (null formatters)))
                       (not (+local-save-format--inhibited-p)))
                  (funcall original interactive)
                formatters))))
      (error (+local-save-format--notice (error-message-string err)) nil)))))

(defun +local-save-format--request-valid-p (request context)
  "Reject stale requests, edits, file changes and environment generation changes."
  (and (eq request +local-save-format--request)
       (equal (plist-get request :file) buffer-file-name)
       (= (plist-get request :tick) (buffer-chars-modified-tick))
       (not (+local-save-format--inhibited-p))
       (or (null (plist-get request :generation))
           (equal (plist-get request :generation) (plist-get context :generation)))
       (let ((current (+local-save-format--context (+local-save-format--source-directory))))
         (and (memq (plist-get current :status) '(ready unmanaged remote))
              (equal (plist-get current :generation) (plist-get context :generation))))))

(defun +local-save-format--request-format (formatters function &optional resolve)
  "Prepare FORMATTERS then call FUNCTION for the latest unchanged request.
When RESOLVE is non-nil, reselect automatic formatters after environment readiness."
  (let* ((buffer (current-buffer))
         (initial-context (+local-save-format--context (+local-save-format--source-directory)))
         (request (list :file buffer-file-name :tick (buffer-chars-modified-tick)
                        :generation (when (memq (plist-get initial-context :status) '(ready unmanaged))
                                      (plist-get initial-context :generation))))
         (selection (or +local-save-format--selection (list :formatters formatters))))
    (setq +local-save-format--request request)
    (cl-labels
        ((finish (context ready)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (if (not ready)
                   (when (eq request +local-save-format--request)
                     (+local-save-format--notice
                      (or (plist-get context :reason) "项目工具尚不可用；内容已普通保存。")))
                 (when (+local-save-format--request-valid-p request context)
                   (setq +local-save-format--request nil +local-save-format--last-notice nil)
                   (let ((+local-save-format--prepared t)
                         (+local-save-format--prepared-selection selection)
                         (+local-save-format--prepared-resources
                          (list :rust-metadata +local-save-format--rust-metadata
                                :dart-stdin +local-save-format--dart-stdin-supported))
                         (current-prefix-arg nil))
                     (+local-save-format--call-context
                      context
                      (lambda ()
                        (let ((default-directory (or (plist-get selection :directory)
                                                     (+local-save-format--source-directory))))
                          (funcall function))))))))))
         (prepare (context)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (cond
                ((not (memq (plist-get context :status) '(ready unmanaged remote)))
                 (finish context nil))
                ((+local-save-format--request-valid-p request context)
                 (when resolve
                   (+local-save-format--call-context
                    context
                    (lambda ()
                      (setq formatters (apheleia--get-formatters)
                            selection +local-save-format--selection))))
                 (if formatters
                     (+local-save-format--prepare formatters #'finish)
                   (setq +local-save-format--request nil))))))))
      (if (and (+local-save-format--env-p) (fboundp '+local-env-ensure))
          (+local-env-ensure (+local-save-format--source-directory) #'prepare)
        (prepare initial-context)))))

(defun +local-save-format--after-save-a (original &rest args)
  "Write first, defer prerequisite discovery, then reuse native guarded formatting."
  (cond
   ((or (bound-and-true-p apheleia-format-after-save-in-progress)
        (not (bound-and-true-p apheleia-mode)) (buffer-narrowed-p)
        (+local-save-format--inhibited-p) (not (memq current-prefix-arg '(nil 1))))
    (setq +local-save-format--request nil))
   ((or (not (+local-save-format--env-p))
        (file-remote-p (or buffer-file-name default-directory)))
    (apply original args))
   (t (let ((formatters (apheleia--get-formatters)))
        (when (or formatters
                  (eq (plist-get (+local-save-format--context (+local-save-format--source-directory)) :status)
                      'pending))
          (+local-save-format--request-format formatters (lambda () (apply original args)) t))))))

(defun +local-save-format--buffer-a (original formatter &rest args)
  "Keep explicit format commands asynchronous while preparing project prerequisites."
  (cond
   ((or (null formatter) (+local-save-format--inhibited-p)) nil)
   ((not (+local-save-format--env-p)) (apply original formatter args))
   (+local-save-format--prepared (+local-save-format--native-format original formatter args))
   ((file-remote-p (or buffer-file-name default-directory))
    (let ((apheleia-formatters +local-save-format--native-formatters))
      (apply original formatter args)))
   (t (+local-save-format--request-format
       (if (listp formatter) formatter (list formatter))
       (lambda () (+local-save-format--native-format original formatter args))))))

(defun +local-save-format--native-format (original formatter args)
  "Use native async formatting and notify failures without exposing process output."
  (let ((buffer (current-buffer)) (success (car args))
        (callback (plist-get (cdr args) :callback)))
    (apply original formatter success :callback
           (lambda (&rest result)
             (when-let ((error (plist-get result :error)))
               (when (and (buffer-live-p buffer)
                          (not (string-match-p "Contents have changed\\|interrupted" (format "%s" error))))
                 (with-current-buffer buffer
                   (+local-save-format--notice "格式器失败；已保留普通保存内容。"))))
             (when callback (apply callback result)))
           (cl-loop for (key value) on (cdr args) by #'cddr
                    unless (eq key :callback) append (list key value)))))

(defun +local-save-format--clean-save-a (original &rest args)
  "Apply manual trimming and formatting after an earlier idle save."
  (let ((manual (or (car args) (called-interactively-p 'any))))
    (when (and manual buffer-file-name (not (buffer-modified-p))
               (not (bound-and-true-p super-save-in-progress))
               (bound-and-true-p +local-save-format--trim-mode))
      (+local-save-format--trim-h))
    (let ((clean (not (buffer-modified-p))) (result (apply original args)))
      (when (and clean manual buffer-file-name (not (buffer-modified-p))
                 (bound-and-true-p apheleia-mode) (not buffer-read-only)
                 (not (buffer-narrowed-p))
                 (not (bound-and-true-p apheleia-format-after-save-in-progress)))
        (apheleia-format-after-save))
      result)))

(defun +local-save-format--save-if-changed-a (original &rest args)
  "Avoid a second native save when the formatter did not alter contents."
  (when (buffer-modified-p) (apply original args)))

(defun +local-save-format--process-context-a (original &rest args)
  "Copy the native formatter process environment to its callback buffers."
  (let ((environment (copy-sequence process-environment))
        (paths (copy-sequence exec-path)) (directory default-directory))
    (dolist (key '(:stdout :stderr))
      (when-let ((buffer (plist-get args key)))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (setq-local process-environment (copy-sequence environment)
                        exec-path (copy-sequence paths) default-directory directory)))))
    (apply original args)))

(defun +local-save-format--environment-changed-h (old new)
  "Discard old environment requests and warm metadata for the new environment."
  (unless (equal (plist-get old :generation) (plist-get new :generation))
    ;; A save made while the initial environment was pending may finish when
    ;; that first resolution becomes ready.  Already-ready generations expire.
    (when-let ((generation (plist-get +local-save-format--request :generation)))
      ;; A newly subscribed buffer may already have selected this shared context.
      (unless (equal generation (plist-get new :generation))
        (setq +local-save-format--request nil)))
    (setq +local-save-format--rust-metadata nil
          +local-save-format--dart-stdin-supported nil)
    (when (and (boundp 'apheleia--current-process)
               (process-live-p apheleia--current-process))
      (process-put apheleia--current-process :interrupted t)
      (interrupt-process apheleia--current-process)))
  (+local-save-format--prefetch))

(defun +local-save-format--setup ()
  "Install project adapters and reload-safe native integration."
  (unless +local-save-format--native-formatters
    (setq +local-save-format--native-formatters (copy-tree apheleia-formatters)))
  (when (+local-save-format--env-p)
    (dolist (adapter +local-save-format-adapters)
      (let ((tool (car adapter)))
        (setf (alist-get tool apheleia-formatters)
              `((+local-save-format-tool-executable ',tool)
                ,@(plist-get (cdr adapter) :arguments)
                (+local-save-format--script-options ',tool)))))
    (dolist (entry apheleia-formatters)
      (when (and (string-prefix-p "prettier-" (symbol-name (car entry)))
                 (consp (cdr entry)) (equal (cadr entry) "apheleia-npx")
                 (equal (caddr entry) "prettier"))
        (setcdr entry (cons '(+local-save-format-tool-executable 'prettier) (cdddr entry)))))
    (setf (alist-get 'rustfmt apheleia-formatters)
          '((or (executable-find "rustfmt") (error "当前项目中找不到 rustfmt"))
            "--quiet" "--emit" "stdout" "--edition" (+local-save-format--rust-edition)
            (when-let ((directory (+local-save-format--rust-config-directory)))
              (list "--config-path" directory))))
    (setf (alist-get 'dart-format apheleia-formatters)
          '((+local-save-format--dart-executable) "format" (+local-save-format--dart-stdin-arguments))))
  (dolist (pair '((apheleia--get-formatters . +local-save-format--get-formatters-a)
                  (apheleia-format-after-save . +local-save-format--after-save-a)
                  (apheleia-format-buffer . +local-save-format--buffer-a)
                  (basic-save-buffer . +local-save-format--clean-save-a)
                  (apheleia--save-buffer-silently . +local-save-format--save-if-changed-a)
                  (apheleia--make-process . +local-save-format--process-context-a)))
    (when (and (fboundp (car pair)) (not (advice-member-p (cdr pair) (car pair))))
      (advice-add (car pair) :around (cdr pair))))
  (when (+local-save-format--env-p)
    (add-hook '+local-env-changed-hook #'+local-save-format--environment-changed-h))
  (add-hook 'hack-local-variables-hook #'+local-save-format--prefetch))
