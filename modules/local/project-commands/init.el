;;; local/project-commands/init.el -*- lexical-binding: t; -*-
(defgroup local-project-commands nil "Defaults for Doom's native project commands." :group 'tools)
(defcustom +local-project-retry-interval 30
  "Seconds before a later preparation or interactive command retries a failed query."
  :type 'natnum :group 'local-project-commands)
(defvar-local +local-project--directory nil)
(defvar-local +local-project--defaults nil)
(defvar-local +local-project--assigned nil)
(defvar-local +local-project--originals nil
  "Values displaced by this module, used only to restore its owned defaults.")
(defvar +local-project--running nil)
(defvar +local-project--queries (make-hash-table :test #'equal))
(defvar +local-project--metadata (make-hash-table :test #'equal))
