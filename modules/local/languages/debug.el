;;; local/languages/debug.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'compile)
(defvar flutter-buffer-name)
(defvar dap--debug-providers)
(defvar +local-languages--cargo-states (make-hash-table :test #'eq :weakness 'key)
  "Captured Cargo contexts keyed by opaque, configuration-owned tokens.
The native Dape configuration contains no environment values or closures.")
(defvar-local +local-languages--flutter-context nil)
(declare-function dape "dape" (config &optional skip-compile))
(declare-function dap-start-debugging-noexpand "dap-mode" (configuration))

(defun +local-languages--flutter-directory ()
  "Use the nearest pubspec component rather than the repository root."
  (file-truename
   (or (locate-dominating-file default-directory "pubspec.yaml")
       (user-error "No pubspec.yaml above this buffer"))))

(defun +local-languages--flutter-buffer-name (directory)
  "Give each DIRECTORY its own existing Flutter console."
  (let ((legacy (get-buffer "*Flutter*")))
    (if (and legacy
             (with-current-buffer legacy
               (equal (file-truename default-directory) directory)))
        "*Flutter*"
      (format "*Flutter: %s %s*" (file-name-nondirectory (directory-file-name directory))
              (substring (secure-hash 'sha1 directory) 0 8)))))

(defun +local-languages--flutter-in-context (function args directory context)
  "Call original Flutter FUNCTION with ARGS in its component CONTEXT."
  (+local-languages--call
   context
   (lambda ()
     (let* ((default-directory directory)
            (flutter-buffer-name (+local-languages--flutter-buffer-name directory))
            (buffer (get-buffer-create flutter-buffer-name)))
       (with-current-buffer buffer
         (setq-local default-directory directory
                     process-environment (copy-sequence (plist-get context :process-environment))
                     exec-path (copy-sequence (plist-get context :exec-path))))
       (prog1 (apply function args)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq-local +local-languages--flutter-context context
                         process-environment (copy-sequence (plist-get context :process-environment))
                         exec-path (copy-sequence (plist-get context :exec-path))))))))))

(defun +local-languages--flutter-entry (function args control)
  "Keep native Flutter entry points, isolating component sessions and controls."
  (let* ((directory (+local-languages--flutter-directory))
         (buffer (get-buffer (+local-languages--flutter-buffer-name directory)))
         (process (and buffer (get-buffer-process buffer)))
         (existing-context (and buffer (buffer-local-value '+local-languages--flutter-context buffer))))
    (when (and control (not (and process (process-live-p process))))
      (user-error "Flutter is not running in this component"))
    (if (and control existing-context)
        (+local-languages--flutter-in-context function args directory existing-context)
      (+local-languages--guard
       (lambda (&rest arguments)
         (+local-languages--flutter-in-context function arguments directory
                                               (+local-languages--context directory)))
       args nil directory))))

(defun +local-languages--flutter-debug-start (source directory context sdk configuration)
  "Start native CONFIGURATION after a possibly asynchronous device chooser."
  (when (buffer-live-p source)
    (with-current-buffer source
      (+local-languages--call
       context
       (lambda ()
         (let ((default-directory directory)
               (configuration (copy-tree configuration)))
           (setq configuration (plist-put configuration :cwd directory))
           (when sdk
             (setq configuration
                   (plist-put configuration :dap-server-path
                              (append sdk '("debug_adapter")
                                      (when-let ((device (plist-get configuration :deviceId)))
                                        (list "-d" device))))))
           (dap-start-debugging-noexpand configuration)))))))

(defun +local-languages--flutter-dap-entry (configuration)
  "Use the registered native Flutter provider in its originating environment."
  (let ((source (current-buffer)) (directory (+local-languages--flutter-directory)))
    (+local-languages--guard
     (lambda ()
       (let* ((context (+local-languages--context directory))
              (provider (or (gethash "flutter" dap--debug-providers)
                            (user-error "Flutter DAP provider is unavailable")))
              (sdk (and (lsp-dart-dap-use-sdk-debugger-p) (lsp-dart-flutter-command)))
              (prepared (funcall provider configuration))
              (start (apply-partially #'+local-languages--flutter-debug-start
                                      source directory context sdk)))
         (if (functionp prepared) (funcall prepared start) (funcall start prepared))))
     nil nil directory)))

(defun +local-languages--adapter ()
  "Read warmed adapter discovery without launching any external commands."
  (or (when-let ((path (+local-languages--executable "lldb-dap")))
        (cons path "lldb-dap"))
      (+local-languages--debug-value 'adapter
                                     (or (locate-dominating-file default-directory "Cargo.toml")
                                         default-directory))
      (when-let ((path (+local-languages--executable "codelldb"))) (cons path "lldb"))))

(defun +local-languages--json (string)
  "Read native tool JSON with string keys."
  (let ((json-object-type 'alist) (json-array-type 'list)
        (json-key-type 'string) (json-null nil) (json-false nil))
    (json-read-from-string string)))

(defun +local-languages--cargo-target (directory)
  "Infer the current buffer's local Cargo target using locked offline metadata."
  (let* ((source (and buffer-file-name (file-truename buffer-file-name)))
         (metadata (or (+local-languages--debug-value 'cargo directory)
                       (user-error "Cargo debug discovery is not available yet")))
         (members (alist-get "workspace_members" metadata nil nil #'equal))
         (packages (cl-remove-if-not
                    (lambda (package) (member (alist-get "id" package nil nil #'equal) members))
                    (alist-get "packages" metadata nil nil #'equal)))
         (owner (car (sort (cl-remove-if-not
                            (lambda (package)
                              (and source
                                   (string-prefix-p
                                    (file-name-directory (alist-get "manifest_path" package nil nil #'equal))
                                    source))) packages)
                           (lambda (left right)
                             (> (length (alist-get "manifest_path" left nil nil #'equal))
                                (length (alist-get "manifest_path" right nil nil #'equal)))))))
         targets)
    (dolist (package (if owner (list owner) packages))
      (when (member (alist-get "id" package nil nil #'equal) members)
        (dolist (target (alist-get "targets" package nil nil #'equal))
          (when-let ((kind (cl-find-if (lambda (value) (member value '("bin" "example" "test" "lib")))
                                       (alist-get "kind" target nil nil #'equal))))
            (push (list :id (alist-get "id" package nil nil #'equal)
                        :package (alist-get "name" package nil nil #'equal)
                        :manifest (alist-get "manifest_path" package nil nil #'equal)
                        :name (alist-get "name" target nil nil #'equal) :kind kind
                        :source (alist-get "src_path" target nil nil #'equal)
                        :default (equal (alist-get "default_run" package nil nil #'equal)
                                        (alist-get "name" target nil nil #'equal))) targets)))))
    (or (cl-find source targets :key (lambda (target) (plist-get target :source)) :test #'equal)
        (cl-find-if (lambda (target) (plist-get target :default)) targets)
        (let ((bins (cl-remove-if-not (lambda (target) (equal (plist-get target :kind) "bin")) targets)))
          (when (= (length bins) 1) (car bins)))
        (when (= (length targets) 1) (car targets))
        (user-error "Cargo target is ambiguous; use the native Dape :program override"))))

(defun +local-languages--cargo-argv (target)
  "Return the native build argv for inferred TARGET."
  (let ((kind (plist-get target :kind)))
    (append (list "cargo" (if (member kind '("lib" "test")) "test" "build"))
            (when (member kind '("lib" "test")) '("--no-run"))
            (list "--package" (plist-get target :package))
            (if (equal kind "lib") '("--lib")
              (list (concat "--" kind) (plist-get target :name)))
            '("--message-format=json-diagnostic-rendered-ansi"))))

(defun +local-languages--cargo-artifact (state)
  "Read STATE's actual executable from the native compilation buffer."
  (let ((buffer (aref state 3)) (target (aref state 2)) executable)
    (unless (buffer-live-p buffer) (user-error "Cargo compilation output is unavailable"))
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (when (eq (char-after) ?{)
            (let* ((message (ignore-errors (+local-languages--json
                                            (buffer-substring-no-properties (point) (line-end-position)))))
                   (artifact (alist-get "target" message nil nil #'equal)))
              (when (and (equal (alist-get "reason" message nil nil #'equal) "compiler-artifact")
                         (equal (alist-get "package_id" message nil nil #'equal) (plist-get target :id))
                         (equal (alist-get "name" artifact nil nil #'equal) (plist-get target :name))
                         (member (plist-get target :kind) (alist-get "kind" artifact nil nil #'equal))
                         (stringp (alist-get "executable" message nil nil #'equal)))
                (setq executable (alist-get "executable" message nil nil #'equal)))))
          (forward-line 1))))
    (or executable (user-error "Cargo did not report the selected target's executable"))))

(defun +local-languages--cargo-state (configuration)
  "Read the captured Cargo state for CONFIGURATION's opaque token."
  (when-let ((token (plist-get configuration '+local-cargo-state)))
    (or (gethash token +local-languages--cargo-states)
        (user-error "The Cargo debug context is no longer available"))))

(defun +local-languages--rust-config (configuration)
  "Prepare a native Dape Cargo configuration, retaining its compile lifecycle.
Dape runs this transformer again after compilation, but does not evaluate
new value functions added by it.  Resolve the artifact on that second pass."
  (if-let ((state (+local-languages--cargo-state configuration)))
      (if (aref state 2)
          (plist-put (copy-sequence configuration) :program
                     (+local-languages--cargo-artifact state))
        configuration)
    (let* ((directory (or (locate-dominating-file default-directory "Cargo.toml")
                          (user-error "No Cargo.toml above this buffer")))
           (context (+local-languages--context directory)))
      (unless (+local-languages--ready-p context)
        (user-error "Project environment is not ready"))
      (+local-languages--call
       context
       (lambda ()
         (let* ((adapter (or (+local-languages--adapter)
                             (user-error "No installed LLDB debug adapter is available")))
                (explicit (plist-get configuration :program))
                (target (unless explicit (+local-languages--cargo-target directory)))
                (token (make-symbol "cargo-context"))
                (state (vector directory context target nil)))
           (puthash token state +local-languages--cargo-states)
           (setq configuration (copy-tree configuration))
           ;; The adapter inherits the captured environment at process startup.
           ;; Keep user-specified command-env and :env overrides untouched.
           (dolist (entry (list (cons 'command (car adapter))
                                (cons 'command-cwd directory)
                                (cons '+local-cargo-state token)
                                (cons :type (cdr adapter)) (cons :cwd directory)
                                (cons :request "launch")))
             (setq configuration (plist-put configuration (car entry) (cdr entry))))
           (when target
             (setq configuration
                   (plist-put configuration 'compile
                              (mapconcat #'shell-quote-argument
                                         (+local-languages--cargo-argv target) " "))))
           (when (equal (cdr adapter) "lldb")
             (setq configuration (plist-put configuration 'command-args '("--port" :autoport))
                   configuration (plist-put configuration 'port :autoport)))
           configuration))))))

(defun +local-languages--native-compile (state function command)
  "Run native compilation FUNCTION for COMMAND using STATE's captured context."
  (let ((default-directory (aref state 0)))
    (let ((buffer (+local-languages--call (aref state 1) function command)))
      (aset state 3 buffer)
      buffer)))

(defun +local-languages--native-compile-token (token function command)
  "Run native FUNCTION for COMMAND without capturing environment in a closure."
  (let ((state (or (gethash token +local-languages--cargo-states)
                   (user-error "The Cargo debug context is no longer available"))))
    (+local-languages--native-compile state function command)))

(defun +local-languages--dape-compile (function configuration callback)
  "Keep Cargo inside Dape's existing compilation mechanism."
  (if-let ((token (plist-get configuration '+local-cargo-state)))
      (let ((dape-compile-function (apply-partially #'+local-languages--native-compile-token
                                                    token dape-compile-function)))
        (funcall function configuration callback))
    (funcall function configuration callback)))

(defun +local-languages--dart-bindings (flutter lsp)
  "Give classic and native Dart the same pre-existing Doom command bindings."
  (when (fboundp 'map!)
    (dolist (map '(dart-mode-map dart-ts-mode-map))
      (when (and (boundp map) (keymapp (symbol-value map)))
        (when flutter
          (eval `(map! :map ,map :localleader
                       (:prefix ("f" . "flutter")
                                "f" #'flutter-run "q" #'flutter-quit
                                "r" #'flutter-hot-reload "R" #'flutter-hot-restart))))
        (when lsp
          (eval `(map! :map ,map :localleader
                       (:prefix ("t" . "test")
                                "t" #'lsp-dart-run-test-at-point "a" #'lsp-dart-run-all-tests
                                "f" #'lsp-dart-run-test-file "l" #'lsp-dart-run-last-test
                                "v" #'lsp-dart-visit-last-test))))))))

(defun +local-languages--dape-config ()
  "Add a native Rust Dape configuration without replacing a user's entry."
  (when (and (boundp 'dape-adapter-dir) (boundp 'doom-user-dir)
             (equal (file-name-as-directory (expand-file-name dape-adapter-dir))
                    (file-name-as-directory (expand-file-name "debug-adapters/" doom-user-dir))))
    (setq dape-adapter-dir
          (if (fboundp 'doom-profile-data-dir)
              (doom-profile-data-dir t "debug-adapters/")
            (expand-file-name "debug-adapters/" doom-data-dir))))
  (when (boundp 'dape-configs)
    (unless (assq 'rust dape-configs)
      (add-to-list 'dape-configs
                   '(rust modes (rust-mode rustic-mode rust-ts-mode)
                     ensure ignore fn +local-languages--rust-config)))))

(provide '+local-languages-debug)
