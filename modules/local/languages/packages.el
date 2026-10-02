;;; local/languages/packages.el -*- no-byte-compile: t; -*-

;; Native ts modes come from Emacs/Doom language modules.  Only their classic
;; TypeScript fallback and Justfile editing need extra packages here.
(when (modulep! :lang javascript)
  (package! typescript-mode))
(package! just-mode)

