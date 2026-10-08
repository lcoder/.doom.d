;;; local/languages/config.el -*- lexical-binding: t; -*-

(load! "grammar")
(load! "lifecycle")

(defconst +local-languages--directory (dir!)
  "Directory containing this module's lazily loaded support files.")

;; Reload support already used in this session, while keeping cold startup lazy.
(dolist (entry '((+local-languages-debug . "debug")
                 (+local-languages-debug-query . "debug-query")
                 (+local-languages-testing . "testing")
                 (+local-languages-linters . "linters")
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
         '((typescript-mode typescript-ts-mode (typescript) "\\.[cm]?ts\\'")
           (+local-languages--tsx-fallback tsx-ts-mode (tsx) "\\.[tj]sx\\'")
           (js-mode js-ts-mode (javascript jsdoc) "\\.[cm]?js\\'")))
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
(after! lsp-mode
  (when (and (modulep! :lang javascript +lsp) (not (modulep! :tools lsp +eglot)))
    (unless (featurep '+local-languages-linters)
      (load! "linters" +local-languages--directory))
    (+local-languages--register-oxlint))
  (+local-languages--install-compat 'lsp-mode))
(when (and (modulep! :lang javascript +lsp) (not (modulep! :tools lsp +eglot)))
  (after! flycheck
    (unless (featurep '+local-languages-linters)
      (load! "linters" +local-languages--directory))
    (+local-languages--install-compat 'flycheck)))
(after! eglot (+local-languages--install-compat 'eglot))

(defun +local-languages--rust-condition-gap-p (node parent bol &rest _)
  "Match an empty Rust line between an if condition and its body."
  (and (null node)
       (equal (treesit-node-type parent) "if_expression")
       (when-let* ((condition (treesit-node-child-by-field-name parent "condition"))
                   (body (treesit-node-child-by-field-name parent "consequence")))
         (and (>= bol (treesit-node-end condition))
              (< bol (treesit-node-start body))))))

(defun +local-languages--rust-last-code-child (node)
  "Return NODE's last child, ignoring extra nodes such as comments."
  (let ((index (1- (treesit-node-child-count node))) child)
    (while (and (>= index 0)
                (progn
                  (setq child (treesit-node-child node index))
                  (treesit-node-check child 'extra)))
      (setq index (1- index)))
    (and (>= index 0) child)))

(defun +local-languages--rust-unfinished-expression-p (node)
  "Return non-nil when NODE clearly needs an expression continuation."
  (let ((last (+local-languages--rust-last-code-child node)))
    (cond
     ((equal (treesit-node-type node) "let_declaration")
      (and (treesit-node-child-by-field-name node "value")
           (equal (treesit-node-type last) ";")
           (treesit-node-check last 'missing)))
     ;; The grammar recovers `let value =` as an ERROR rather than a let.
     ;; Accept only its declaration prefix, not arbitrary recovery nodes.
     ((equal (treesit-node-type node) "ERROR")
      (let* ((children (cl-loop for index below (treesit-node-child-count node)
                                for child = (treesit-node-child node index)
                                unless (treesit-node-check child 'extra)
                                collect child))
             (types (mapcar #'treesit-node-type children)))
        (and (equal (car types) "let")
             (equal (car (last types)) "=")
             (not (cl-some (lambda (child) (treesit-node-check child 'has-error))
                           children))
             (progn
               (setq types (cdr (butlast types)))
               (when (equal (car types) "mutable_specifier")
                 (setq types (cdr types)))
               (and (string-match-p "\\`\\(?:identifier\\|.*_pattern\\)\\'"
                                    (or (car types) ""))
                    (or (= (length types) 1)
                        (and (= (length types) 3)
                             (equal (cadr types) ":")
                             (treesit-node-check (nth (- (length children) 2) children)
                                                 'named))))))))
     (t
      ;; Follow only the trailing branch: an unrelated earlier parse error
      ;; must not turn a complete expression into a continuation.
      (let ((tail node) missing)
        (while (setq last (+local-languages--rust-last-code-child tail))
          (setq tail last))
        (setq missing (treesit-node-check tail 'missing))
        (when missing
          (let ((position (treesit-node-start tail)))
            (while (and tail (not (treesit-node-eq tail node))
                        (not (and
                              (member (treesit-node-type tail)
                                      '("assignment_expression" "compound_assignment_expr"
                                        "binary_expression"))
                              (when-let ((right (treesit-node-child-by-field-name tail "right")))
                                (and (<= (treesit-node-start right) position)
                                     (>= (treesit-node-end right) position))))))
              (setq tail (treesit-node-parent tail)))
            (and tail
                 (member (treesit-node-type tail)
                         '("assignment_expression" "compound_assignment_expr"
                           "binary_expression"))))))))))

(defun +local-languages--rust-continuation-node (bol)
  "Find a clearly unfinished block-level Rust expression before BOL."
  (save-excursion
    (goto-char bol)
    (skip-chars-backward " \t\n\r")
    (when (> (point) (point-min))
      (let ((node (treesit-node-at (1- (point)) 'rust)) parent)
        ;; A standalone comment line keeps its existing indentation behavior.
        (unless (and (treesit-node-check node 'extra)
                     (= (treesit-node-start node)
                        (save-excursion
                          (goto-char (treesit-node-start node))
                          (back-to-indentation)
                          (point))))
          (while (and node
                      (setq parent (treesit-node-parent node))
                      (or (not (equal (treesit-node-type parent) "block"))
                          ;; A closing brace can finish a let initializer;
                          ;; find the declaration outside that finished block.
                          (equal (treesit-node-type node) "}")))
            (setq node parent))
          (and parent
               (equal (treesit-node-type parent) "block")
               (<= (treesit-node-end node) bol)
               (+local-languages--rust-unfinished-expression-p node)
               node))))))

(defun +local-languages--rust-continuation-gap-p (node _parent bol &rest _)
  "Match an empty Rust line after a clearly unfinished expression."
  (and (null node) (+local-languages--rust-continuation-node bol)))

(defun +local-languages--rust-continuation-anchor (_node _parent bol &rest _)
  "Anchor an empty Rust continuation at its expression's initial line."
  (save-excursion
    (goto-char (treesit-node-start (+local-languages--rust-continuation-node bol)))
    (back-to-indentation)
    (point)))

(defun +local-languages--rust-indent-setup-h ()
  "Align Rust blocks and indent empty condition and expression continuations."
  (when (and (derived-mode-p 'rust-ts-mode)
             (eq indent-line-function #'treesit-indent)
             (boundp 'treesit-simple-indent-override-rules))
    (setq-local treesit-simple-indent-override-rules
                (copy-tree treesit-simple-indent-override-rules))
    (cl-pushnew '((node-is "block") parent-bol 0)
                (alist-get 'rust treesit-simple-indent-override-rules)
                :test #'equal)
    (cl-pushnew '(+local-languages--rust-condition-gap-p parent-bol rust-ts-indent-offset)
                (alist-get 'rust treesit-simple-indent-override-rules)
                :test #'equal)
    (cl-pushnew '(+local-languages--rust-continuation-gap-p
                  +local-languages--rust-continuation-anchor rust-ts-indent-offset)
                (alist-get 'rust treesit-simple-indent-override-rules)
                :test #'equal)))

(when (modulep! :lang rust +tree-sitter)
  (after! rust-ts-mode
    (add-hook 'rust-ts-mode-hook #'+local-languages--rust-indent-setup-h)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (+local-languages--rust-indent-setup-h)))))

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
