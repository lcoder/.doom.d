;;; local/languages/init.el -*- lexical-binding: t; -*-

(defvar +local-languages--env-enabled nil
  "Whether language services should use the local env module's public API.")
(defvar +local-languages--grammar-jobs (make-hash-table :test #'eq)
  "Machine-local grammar preparation state, indexed by language.")
(defvar +local-languages--server-jobs (make-hash-table :test #'equal)
  "Language-server preparation state, indexed by server and environment.")
(defvar +local-languages--lsp-environments (make-hash-table :test #'eq :weakness 'key)
  "Environment identities of native LSP workspaces.")
(defvar +local-languages--retired-workspaces (make-hash-table :test #'eq :weakness 'key)
  "Workspaces already retired following an environment change.")

