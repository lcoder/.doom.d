;;; local/languages/linters.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defvar-local +local-languages-oxlint-config-path nil
  "Optional Oxlint config path, relative to the LSP workspace root.
Set this with directory-local variables for nonstandard config names.
Nil leaves config discovery and nested configuration to Oxlint.")
(put '+local-languages-oxlint-config-path 'safe-local-variable
     (lambda (value) (or (null value) (stringp value))))

(defvar-local +local-languages--lint-state nil
  "Project lint declarations and the installed project executable.")
(defvar-local +local-languages--oxlint-notice nil
  "The project already reported as missing its Oxlint executable.")

(defconst +local-languages--oxlint-configs
  '(".oxlintrc.json" ".oxlintrc.jsonc" "oxlint.config.ts" "oxlint.config.mts")
  "Default configuration names discovered by Oxlint.")
(defconst +local-languages--eslint-configs
  '("eslint.config.js" "eslint.config.mjs" "eslint.config.cjs"
    "eslint.config.ts" "eslint.config.mts" "eslint.config.cts"
    ".eslintrc" ".eslintrc.js" ".eslintrc.cjs" ".eslintrc.json"
    ".eslintrc.yaml" ".eslintrc.yml")
  "Flat and legacy configuration names that explicitly enable ESLint.")

(defun +local-languages--js-file-p ()
  "Whether this is a local JS/TS file, independently of its major mode."
  (and buffer-file-name
       (not (file-remote-p buffer-file-name))
       (string-match-p "\\.\\(?:[cm]?[jt]s\\|[jt]sx\\)\\'" buffer-file-name)))

(defun +local-languages--lint-directories ()
  "Find enclosing directories without escaping the Doom project root."
  (let* ((directory (file-name-directory (expand-file-name buffer-file-name)))
         (root (file-name-as-directory
                (expand-file-name
                 (or (and (fboundp 'doom-project-root) (doom-project-root))
                     (locate-dominating-file directory "package.json")
                     directory))))
         directories)
    (unless (string-prefix-p root directory)
      (setq root directory))
    (while directory
      (push directory directories)
      (setq directory
            (unless (equal directory root)
              (file-name-directory (directory-file-name directory)))))
    (nreverse directories)))

(defun +local-languages--lint-package (directory)
  "Read DIRECTORY's package declarations without executing project commands."
  (let ((file (expand-file-name "package.json" directory)))
    (when (file-readable-p file)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (json-parse-buffer :object-type 'alist :array-type 'list
                               :null-object nil :false-object nil))
        (error nil)))))

(defun +local-languages--lint-config-p (directory names)
  "Whether DIRECTORY contains a regular configuration file from NAMES."
  (cl-some (lambda (name) (file-regular-p (expand-file-name name directory))) names))

(defun +local-languages--refresh-linters ()
  "Refresh declarations at file-open or LSP-start time, never by running tools."
  (when (+local-languages--js-file-p)
    (let* ((directories (+local-languages--lint-directories))
           (root (car (last directories)))
           (oxlint (and +local-languages-oxlint-config-path t))
           eslint executable)
      (dolist (directory directories)
        (let ((package (+local-languages--lint-package directory))
              (binary (expand-file-name "node_modules/.bin/oxlint" directory)))
          (setq oxlint
                (or oxlint
                    (+local-languages--lint-config-p directory +local-languages--oxlint-configs)
                    (cl-some (lambda (key) (assq 'oxlint (alist-get key package)))
                             '(dependencies devDependencies optionalDependencies peerDependencies)))
                eslint
                (or eslint
                    (+local-languages--lint-config-p directory +local-languages--eslint-configs)
                    (assq 'eslintConfig package)))
          (when (and (not executable) (file-regular-p binary) (file-executable-p binary))
            (setq executable binary))))
      (setq +local-languages--lint-state
            (list :root root :oxlint (and oxlint t) :eslint (and eslint t)
                  :executable executable))
      (when executable (setq +local-languages--oxlint-notice nil)))))

(defun +local-languages--linter-selected-p (client)
  "Honor an explicit LSP allow-list before automatic project declarations."
  (unless +local-languages--lint-state (+local-languages--refresh-linters))
  (if (and (boundp 'lsp-enabled-clients) lsp-enabled-clients)
      (memq client lsp-enabled-clients)
    (plist-get +local-languages--lint-state
               (if (eq client 'oxlint) :oxlint :eslint))))

(defun +local-languages--lint-client-p (function client)
  "Filter only the ESLint add-on for local JS/TS buffers."
  (if (and (+local-languages--js-file-p)
           (eq (lsp--client-server-id client) 'eslint))
      (and (+local-languages--linter-selected-p 'eslint) (funcall function client))
    (funcall function client)))

(defun +local-languages--lint-checker-p (function checker)
  "Avoid standalone ESLint without a declaration; honor explicit checker choice."
  (if (and (+local-languages--js-file-p) (eq checker 'javascript-eslint)
           (not (eq flycheck-checker 'javascript-eslint)))
      (and (+local-languages--linter-selected-p 'eslint) (funcall function checker))
    (funcall function checker)))

(defun +local-languages--oxlint-present-p ()
  "Check the cached local executable without installing or probing a tool."
  (let ((executable (plist-get +local-languages--lint-state :executable)))
    (if (and executable (file-executable-p executable))
        t
      (let ((root (plist-get +local-languages--lint-state :root)))
        (unless (equal root +local-languages--oxlint-notice)
          (setq +local-languages--oxlint-notice root)
          (message "Oxlint 未安装：请在 %s 安装项目依赖，然后重新连接 LSP" root)))
      nil)))

(defun +local-languages--oxlint-command ()
  "Start the installed project version, using the existing environment guard."
  (list (or (plist-get +local-languages--lint-state :executable)
            (user-error "未找到项目内的 Oxlint 可执行文件"))
        "--lsp"))

(defun +local-languages--register-oxlint ()
  "Register one native add-on and its workspace configuration, safely on reload."
  (lsp-register-custom-settings
   '(("oxc_language_server.run" "onType")
     ("oxc_language_server.configPath" +local-languages-oxlint-config-path)))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection #'+local-languages--oxlint-command
                                          #'+local-languages--oxlint-present-p)
    :activation-fn (lambda (&rest _)
                     (and (+local-languages--js-file-p)
                          (+local-languages--linter-selected-p 'oxlint)))
    :server-id 'oxlint
    :priority -1
    :add-on? t
    :multi-root nil)))

(add-hook 'find-file-hook #'+local-languages--refresh-linters)
(add-hook 'hack-local-variables-hook #'+local-languages--refresh-linters)

(provide '+local-languages-linters)
