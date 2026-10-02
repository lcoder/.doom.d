;;; local/languages/autoload/compat.el -*- lexical-binding: t; -*-

;; All private/package compatibility advice is installed in this one place.

(defun +local-languages--flutter-run-a (function &rest args)
  "Keep the native Flutter run command inside its component."
  (+local-languages--flutter-entry function args nil))

(defun +local-languages--flutter-control-a (function &rest args)
  "Send native control commands only to this component's existing process."
  (+local-languages--flutter-entry function args t))

(defun +local-languages--dart-test-run-a (function &rest args)
  "Guard SDK resolution and preserve native test processes per component.
The installed lsp-dart uses one global process buffer and resolves its root
inside that buffer; binding only the source environment cannot isolate runs."
  (+local-languages--dart-test-entry function args))

(defun +local-languages--flutter-debug-a (_function path)
  "Handle both synchronous and asynchronous registered Flutter providers."
  (+local-languages--flutter-dap-entry (list :type "flutter" :program path :noDebug nil)))

(defun +local-languages--flutter-dap-run-a (_function &optional path args)
  "Preserve the existing lsp-dart non-debug run entry point."
  (+local-languages--flutter-dap-entry
   (append (list :type "flutter" :name "Flutter Run" :program path :noDebug t)
           (when args (list :args args)))))

(defun +local-languages--dape-entry-a (function &rest args)
  "Resume native Dape with its original environment after Cargo compilation."
  (let* ((configuration (car args))
         (state (+local-languages--cargo-state configuration))
         (cargo (or state (eq (plist-get configuration 'fn)
                              '+local-languages--rust-config)))
         (directory (if cargo (or (and state (aref state 0))
                                  (locate-dominating-file default-directory "Cargo.toml")
                                  default-directory) default-directory)))
    (if state
        (let ((default-directory directory))
          (apply #'+local-languages--call (aref state 1) function args))
      (+local-languages--guard
       (if cargo
           (lambda (&rest arguments) (+local-languages--dape-ready function arguments directory))
         function)
       args nil directory))))

;;;###autoload
(defun +local-languages--install-compat (feature)
  "Install idempotent, capability-gated compatibility advice for FEATURE."
  (let ((specs
         (pcase feature
           ('lsp-mode
            '((lsp :around +local-languages--guard-lsp)
              (lsp-deferred :around +local-languages--guard-lsp)
              (lsp--start-connection :filter-return +local-languages--remember-workspace)
              (lsp--session-workspaces :filter-return +local-languages--filter-workspaces)
              (lsp--find-workspace :around +local-languages--find-workspace)
              (lsp--find-multiroot-workspace :around +local-languages--find-multiroot)
              (lsp--try-open-in-library-workspace :around +local-languages--find-multiroot)))
           ('eglot '((eglot-ensure :around +local-languages--guard-eglot)))
           ('flutter
            '((flutter-run :around +local-languages--flutter-run-a)
              (flutter-hot-reload :around +local-languages--flutter-control-a)
              (flutter-hot-restart :around +local-languages--flutter-control-a)
              (flutter-quit :around +local-languages--flutter-control-a)))
           ('lsp-dart-test-support
            '((lsp-dart-test--run :around +local-languages--dart-test-run-a)))
           ('lsp-dart-dap
            '((lsp-dart-dap-debug-flutter :around +local-languages--flutter-debug-a)
              (lsp-dart-dap-run-flutter :around +local-languages--flutter-dap-run-a)))
           ('dape '((dape :around +local-languages--dape-entry-a)
                    (dape--compile :around +local-languages--dape-compile))))))
    (dolist (spec specs)
      (when (and (fboundp (car spec)) (fboundp (nth 2 spec))
                 (or (not (memq (car spec) '(lsp--find-workspace lsp--find-multiroot-workspace
                                             lsp--try-open-in-library-workspace)))
                     (fboundp 'lsp-session-folder->servers))
                 (not (advice-member-p (nth 2 spec) (car spec))))
        (advice-add (car spec) (nth 1 spec) (nth 2 spec))))))

(provide '+local-languages-compat)
