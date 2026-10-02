;;; local/languages/testing.el -*- lexical-binding: t; -*-

(defvar lsp-dart-test--process-buffer-name)
(defvar lsp-dart-test-output--buffer-name)
(defvar lsp-dart-test-output--show-loading-tests-message)

(defun +local-languages--dart-test-buffer-name (directory &optional output)
  "Name DIRECTORY's native test process or OUTPUT buffer."
  (format "*LSP Dart tests%s: %s %s*" (if output "" " process")
          (file-name-nondirectory (directory-file-name directory))
          (substring (secure-hash 'sha1 directory) 0 8)))

(defun +local-languages--dart-test-call (function args directory context)
  "Delegate FUNCTION and ARGS to native tests in component DIRECTORY/CONTEXT."
  (+local-languages--call
   context
   (lambda ()
     (let* ((default-directory directory)
            ;; The native process mode sets this globally.  Keep its pipe
            ;; choice inside this run so later Doom/comint commands retain PTYs.
            (process-connection-type nil)
            (lsp-dart-test--process-buffer-name
             (+local-languages--dart-test-buffer-name directory))
            (lsp-dart-test-output--buffer-name
             (+local-languages--dart-test-buffer-name directory t))
            (lsp-dart-test-output--show-loading-tests-message t)
            (process-buffer (get-buffer-create lsp-dart-test--process-buffer-name)))
       ;; Native --run-process switches buffers before choosing cwd and starts
       ;; its mode before spawning.  Initialize that mode first, then copy the
       ;; snapshot so mode initialization cannot discard the local environment.
       (with-current-buffer process-buffer
         (setq-local default-directory directory)
         (let ((process-environment (copy-sequence (plist-get context :process-environment)))
               (exec-path (copy-sequence (plist-get context :exec-path))))
           (unless (derived-mode-p 'lsp-dart-test-process-mode)
             (lsp-dart-test-process-mode)))
         (setq-local default-directory directory
                     process-environment (copy-sequence (plist-get context :process-environment))
                     exec-path (copy-sequence (plist-get context :exec-path))
                     lsp-dart-test--process-buffer-name (buffer-name process-buffer)
                     lsp-dart-test-output--buffer-name (+local-languages--dart-test-buffer-name directory t)
                     lsp-dart-test--running-tests nil
                     lsp-dart-test-output--tests-count 0
                     lsp-dart-test-output--tests-passed 0
                     lsp-dart-test-output--show-loading-tests-message t))
       (when (fboundp 'lsp-dart-test-output-content-mode)
         (with-current-buffer (get-buffer-create lsp-dart-test-output--buffer-name)
           (unless (derived-mode-p 'lsp-dart-test-output-content-mode)
             (lsp-dart-test-output-content-mode))
           (setq-local default-directory directory)))
       (apply function args)))))

(defun +local-languages--dart-test-entry (function args)
  "Prepare the native test entry before it resolves SDKs or builds a command."
  (let* ((test-file (plist-get (car args) :file-name))
         (directory
          (let ((default-directory (if test-file (file-name-directory test-file)
                                     default-directory)))
            (+local-languages--flutter-directory))))
    (+local-languages--guard
     (lambda (&rest arguments)
       (+local-languages--dart-test-call function arguments directory
                                         (+local-languages--context directory)))
     args nil directory)))

(provide '+local-languages-testing)
