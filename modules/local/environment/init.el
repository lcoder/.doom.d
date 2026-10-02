;;; local/environment/init.el -*- lexical-binding: t; -*-

(defgroup +local-env nil "Asynchronous project environments." :group 'tools)
(defcustom +local-env-command-timeout 10
  "Seconds allowed for one asynchronous mise query."
  :type 'number :group '+local-env)
(defcustom +local-env-recheck-interval 30
  "Idle seconds between lightweight checks of active project contexts."
  :type 'number :group '+local-env)
(defcustom +local-env-retry-interval 60
  "Minimum seconds before retrying an unchanged unavailable environment."
  :type 'number :group '+local-env)
(defcustom +local-env-cache-ttl 300
  "Seconds before re-resolving an active context despite unchanged files."
  :type 'number :group '+local-env)
(defcustom +local-env-tool-overrides nil
  "Alist of executable names to machine-local paths.
The legacy `my/dev-tool-overrides' remains supported after this alist."
  :type '(alist :key-type string :value-type file) :group '+local-env)
(defvar my/dev-tool-overrides nil)
(defvar +local-env-changed-hook nil
  "Hook called with OLD and NEW settled contexts in each affected live buffer.
Consumers decide whether to refresh their own services; this module starts none.")
(defvar +local-env--cache (make-hash-table :test #'equal))
(defvar +local-env--directories (make-hash-table :test #'equal))
(defvar +local-env--requests (make-hash-table :test #'equal))
(defvar +local-env--watchers (make-hash-table :test #'equal))
(defvar +local-env--dirty (make-hash-table :test #'equal))
(defvar +local-env--notices (make-hash-table :test #'equal))
(defvar +local-env--base-environment nil)
(defvar +local-env--base-exec-path nil)
(defvar +local-env--generation 0)
(defvar +local-env--serial 0)
(defvar +local-env--idle-timer nil)
(defvar +local-env--change-timer nil)
(defvar +local-env-mode nil)
(defvar-local +local-env--buffer-directory nil)
(defvar-local +local-env--buffer-key nil)
(defvar-local +local-env--applied-context nil)
(defvar-local +local-env--native-environment nil)
(defvar-local +local-env--native-exec-path nil)
(defvar-local +local-env--native-local-environment-p nil)
(defvar-local +local-env--native-local-exec-path-p nil)
