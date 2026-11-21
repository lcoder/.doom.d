;;; +org-src.el --- Native language faces in Org blocks -*- lexical-binding: t; -*-

(with-eval-after-load 'org-src
  ;; Source block display only needs Rust's font-lock rules.
  (setq rust-load-optional-libraries nil)
  ;; Standalone Neomacs can load the same mode libraries without Doom autoloads.
  (dolist (mode '(rust-mode dart-mode typescript-mode yaml-mode markdown-mode))
    (let ((library (symbol-name mode)))
      (when (and (not (fboundp mode)) (locate-library library))
        (autoload mode library nil t))))
  ;; Org appends "-mode" to the value.  All other names retain Org's lookup.
  (dolist (entry '(("rs" . rust) ("ts" . typescript) ("jsx" . js-jsx)
                   ("tsx" . js-jsx) ("json" . js-json) ("yml" . yaml)
                   ("md" . markdown) ("xml" . nxml)))
    (setf (alist-get (car entry) org-src-lang-modes nil nil #'equal) (cdr entry))))

;;; +org-src.el ends here
