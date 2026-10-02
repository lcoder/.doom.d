;;; local/save-format/init.el -*- lexical-binding: t; -*-

(defgroup +local-save-format nil "Project-aware saving and formatting." :group 'tools)

(defcustom +local-save-format-adapters
  '((oxfmt :package "oxfmt" :executable "oxfmt"
     :config-files (".oxfmtrc.json" ".oxfmtrc.jsonc" "oxfmt.config.ts"
                    "oxfmt.config.js" "oxfmt.config.mjs")
     :arguments ("--stdin-filepath" filepath))
    (prettier :package "prettier" :executable "prettier"
              :package-key "prettier"
              :config-files (".prettierrc" ".prettierrc.json" ".prettierrc.yml"
                             ".prettierrc.yaml" ".prettierrc.json5" ".prettierrc.js"
                             ".prettierrc.mjs" ".prettierrc.cjs" ".prettierrc.toml"
                             "prettier.config.js" "prettier.config.mjs"
                             "prettier.config.cjs" "prettier.config.ts"
                             "prettier.config.mts" "prettier.config.cts")
              :arguments ("--stdin-filepath" filepath))
    (biome :package "@biomejs/biome" :executable "biome"
           :config-files ("biome.json" "biome.jsonc")
           :arguments ("format" "--stdin-file-path" filepath)))
  "Installed stdin formatters and the project evidence identifying them.
Add an adapter to support another tool without changing dispatch code.
Commands are registered in `apheleia-formatters'; script bodies are never run."
  :type '(repeat sexp))

(defcustom +local-save-format-resolvers '(+local-save-format-js-project)
  "Project resolvers, called in order until one returns a result plist.
A result contains :formatters, optional :directory and :reason.  A non-nil
:blocked skips formatting; nil means this resolver does not apply."
  :type '(repeat function))

(defvar-local my/dev-format-choice 'auto
  "Explicit formatter override: auto, nil (disabled), or formatter symbols.
Compatibility with earlier local configuration only; prefer apheleia-formatter.
Existing `apheleia-formatter', `+format-with' and inhibit variables still work.")
(put 'my/dev-format-choice 'safe-local-variable
     (lambda (value)
       (or (null value) (symbolp value)
           (and (listp value) (cl-every #'symbolp value)))))


(defcustom +local-save-format-prepare-timeout 8
  "Seconds allowed for a background formatter prerequisite."
  :type 'number :group '+local-save-format)
