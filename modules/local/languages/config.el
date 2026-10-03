;;; local/languages/config.el -*- lexical-binding: t; -*-

(load! "grammar")
(load! "lifecycle")

(defconst +local-languages--directory (dir!)
  "Directory containing this module's lazily loaded support files.")

;; Reload support already used in this session, while keeping cold startup lazy.
(dolist (entry '((+local-languages-debug . "debug")
                 (+local-languages-debug-query . "debug-query")
                 (+local-languages-testing . "testing")
                 (+local-languages-compat . "autoload/compat")))
  (when (featurep (car entry))
    (load! (cdr entry) +local-languages--directory)))

(setq +local-languages--env-enabled (modulep! :local environment)
      treesit-auto-install-grammar 'never
      +local-languages--specs
      (append
       (when (modulep! :lang dart +tree-sitter)
         '((dart-mode dart-ts-mode (dart) "\\.dart\\'")))
       (when (modulep! :lang rust +tree-sitter)
         '((rust-mode rustic-mode (rust) "\\.rs\\'")))
       (when (modulep! :lang yaml +tree-sitter)
         '((yaml-mode yaml-ts-mode (yaml) "\\.ya?ml\\'")))
       (when (modulep! :lang json +tree-sitter)
         '((json-mode json-ts-mode (json) "\\.jsonc?\\'")))
       (when (modulep! :lang javascript +tree-sitter)
         '((typescript-mode typescript-ts-mode (typescript) "\\.ts\\'")
           (+local-languages--tsx-fallback tsx-ts-mode (tsx) "\\.[tj]sx\\'")
           (js-mode js-ts-mode (javascript jsdoc) "\\.js\\'")))
       (when (modulep! :tools tree-sitter)
         '((conf-toml-mode toml-ts-mode (toml) "\\.toml\\'")))))

(add-hook 'after-change-major-mode-hook #'+local-languages--prepare-buffer)
(add-hook '+local-env-changed-hook #'+local-languages--environment-changed)
(+local-languages--start-recovery)

(after! treesit (+local-languages--register-remaps))
(after! typescript-mode (+local-languages--register-remaps))
(after! dart-mode
  (unless (featurep '+local-languages-debug)
    (load! "debug" +local-languages--directory))
  (+local-languages--dart-bindings (modulep! :lang dart +flutter) (modulep! :lang dart +lsp)))
(after! dart-ts-mode
  (unless (featurep '+local-languages-debug)
    (load! "debug" +local-languages--directory))
  (+local-languages--dart-bindings (modulep! :lang dart +flutter) (modulep! :lang dart +lsp)))
(after! lsp-dart
  (unless (featurep '+local-languages-debug)
    (load! "debug" +local-languages--directory))
  (setq lsp-dart-project-root-discovery-strategies '(closest-pubspec lsp-root))
  (+local-languages--dart-bindings (modulep! :lang dart +flutter) t))
(after! flutter
  (unless (featurep '+local-languages-debug)
    (load! "debug" +local-languages--directory))
  (+local-languages--install-compat 'flutter)
  (set-popup-rule! "^\\*Flutter:" :ttl 0 :quit t)
  (+local-languages--dart-bindings t (modulep! :lang dart +lsp)))
(after! lsp-dart-test-support
  (unless (featurep '+local-languages-debug)
    (load! "debug" +local-languages--directory))
  (unless (featurep '+local-languages-testing)
    (load! "testing" +local-languages--directory))
  (+local-languages--install-compat 'lsp-dart-test-support))
(after! lsp-dart-dap
  (unless (featurep '+local-languages-debug)
    (load! "debug" +local-languages--directory))
  (+local-languages--install-compat 'lsp-dart-dap))
(after! lsp-mode (+local-languages--install-compat 'lsp-mode))
(after! eglot (+local-languages--install-compat 'eglot))
(when (modulep! :lang rust)
  (after! rustic-interaction
    (+local-languages--install-compat 'rustic-interaction))
  (after! rustic-cargo
    (unless (featurep '+local-languages-testing)
      (load! "testing" +local-languages--directory))
    (+local-languages--install-compat 'rustic-cargo)))
(after! lsp-javascript (setq lsp-clients-typescript-prefer-use-project-ts-server t))
(after! dape
  (when (modulep! :lang rust)
    (unless (featurep '+local-languages-debug)
      (load! "debug" +local-languages--directory))
    (unless (featurep '+local-languages-debug-query)
      (load! "debug-query" +local-languages--directory))
    (+local-languages--dape-config)
    (+local-languages--install-compat 'dape)))

(defun +local-languages--warm-debug ()
  "Warm read-only native Rust discovery only for buffers that need it."
  (when (and buffer-file-name (string-match-p "\\.rs\\'" buffer-file-name))
    (when-let ((directory (locate-dominating-file default-directory "Cargo.toml")))
      (let ((context (+local-languages--context directory)))
        (when (+local-languages--ready-p context)
          (unless (featurep '+local-languages-debug)
            (load! "debug" +local-languages--directory))
          (unless (featurep '+local-languages-debug-query)
            (load! "debug-query" +local-languages--directory))
          (+local-languages--debug-discover 'cargo directory context #'ignore)
          (+local-languages--debug-discover 'adapter directory context #'ignore))))))

(when (and (modulep! :tools debugger) (modulep! :lang rust))
  (add-hook 'find-file-hook #'+local-languages--warm-debug))

(use-package! just-mode :mode ("[Jj]ustfile\\'" . just-mode))
(+local-languages--register-remaps)
