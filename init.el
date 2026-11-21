;;; init.el -*- lexical-binding: t; -*-

;; A Gitless package cache must never resolve to Doom's parent repository.
(when (and noninteractive
           (boundp 'doom-local-dir)
           (stringp doom-local-dir))
  (let* ((repos (file-truename
                 (expand-file-name "straight/repos" doom-local-dir)))
         (ceilings (split-string (or (getenv "GIT_CEILING_DIRECTORIES") "")
                                 path-separator t)))
    (setenv "GIT_CEILING_DIRECTORIES"
            (mapconcat #'identity (delete-dups (cons repos ceilings))
                       path-separator))))

(defgroup my/dev nil "Personal Doom preferences." :group 'convenience)

(defcustom my/dev-notes-directory nil
  "Machine-local Org directory, or nil to use an existing notes library."
  :type '(choice (const nil) directory) :group 'my/dev)

(defcustom my/dev-font-families '("Maple Mono NF CN" "SF Mono" "Menlo")
  "Preferred available editor fonts."
  :type '(repeat string) :group 'my/dev)

(defcustom my/dev-font-size 13
  "Editor font size in points."
  :type 'number :group 'my/dev)

(defcustom my/dev-org-font-families '("Maple Mono NF CN")
  "Preferred available fonts for Org buffers."
  :type '(repeat string) :group 'my/dev)

(defun my/load-local-settings-h ()
  "Load local overrides and set the notes root before Org derives its paths."
  (let ((file (expand-file-name "local.el" doom-user-dir)))
    (when (file-readable-p file)
      (load file nil t)))
  (let ((icloud (expand-file-name "Library/Mobile Documents/com~apple~CloudDocs/org/" "~")))
    (setq org-directory
          (expand-file-name
           (cond (my/dev-notes-directory)
                 ((and (bound-and-true-p org-directory)
                       (file-accessible-directory-p org-directory))
                  org-directory)
                 ((file-accessible-directory-p icloud) icloud)
                 (t "~/org/"))))))

(add-hook 'doom-before-modules-config-hook #'my/load-local-settings-h -100)

(doom! :input
       chinese

       :completion
       corfu
       vertico

       :ui
       doom
       modeline
       (popup +defaults)
       (zen +focus)

       :editor
       (evil +everywhere)
       file-templates
       snippets

       :emacs
       dired
       undo

       :os
       (:if (featurep :system 'macos) macos)

       :lang
       (org +roam)

       :config
       (default +bindings +smartparens))
