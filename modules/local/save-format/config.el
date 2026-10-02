;;; local/save-format/config.el -*- lexical-binding: t; -*-

(defconst +local-save-format--directory (dir!)
  "Directory containing this module's deferred formatting support.")

(defun +local-save-format--auto-save-safe-p ()
  "Only idle-save existing, unlocked local plaintext files."
  (and buffer-file-name (not (file-remote-p buffer-file-name))
       (not buffer-read-only) (not (buffer-base-buffer))
       (not (bound-and-true-p epa-file-encrypt-to))
       (not (string-match-p
             (if (boundp 'epa-file-name-regexp) epa-file-name-regexp
               "\\.gpg\\(~\\|\\.~[0-9]+~\\)?\\'") buffer-file-name))
       (file-regular-p buffer-file-name)
       (let ((owner (file-locked-p buffer-file-name))) (or (null owner) (eq owner t)))))

(defun +local-save-format--trim-h ()
  "Honor EditorConfig trimming on manual saves, without changing idle saves."
  (when (and (not buffer-read-only) (not (bound-and-true-p super-save-in-progress)))
    (delete-trailing-whitespace)))

(define-minor-mode +local-save-format--trim-mode
  "Apply the project's EditorConfig whitespace rule during manual saves."
  :lighter nil
  (if +local-save-format--trim-mode
      (add-hook 'before-save-hook #'+local-save-format--trim-h nil t)
    (remove-hook 'before-save-hook #'+local-save-format--trim-h t)))

(after! editorconfig
  (setq editorconfig-trim-whitespaces-mode #'+local-save-format--trim-mode))

(use-package! super-save
  :demand t
  :config
  (when (bound-and-true-p super-save-mode) (super-save-mode -1))
  (setq super-save-auto-save-when-idle t super-save-idle-duration 30
        super-save-all-buffers t super-save-remote-files nil
        super-save-when-focus-lost nil super-save-when-buffer-switched nil
        super-save-triggers nil super-save-hook-triggers nil
        super-save-handle-org-src nil super-save-handle-edit-indirect nil
        super-save-delete-trailing-whitespace nil)
  (add-hook 'super-save-predicates #'+local-save-format--auto-save-safe-p)
  (super-save-mode 1))

(use-package! apheleia
  :defer t
  :config
  (load! "+project" +local-save-format--directory)
  (load! "+prepare" +local-save-format--directory)
  (load! "+save" +local-save-format--directory)
  (+local-save-format--setup))
