;;; local/languages/doctor.el -*- lexical-binding: t; -*-

(when (version< emacs-version "29.1")
  (error! "The local languages module requires Emacs 29.1 or newer"))
(unless (and (fboundp 'treesit-available-p) (treesit-available-p))
  (warn! "Native Tree-sitter is unavailable; classic language modes remain available"))
(unless (and (executable-find "git")
             (or (executable-find "cc") (executable-find "gcc")))
  (warn! "Automatic native grammar preparation needs local Git and a C compiler"))
(when (and (modulep! :tools lsp +eglot) (not (featurep 'eglot)))
  (warn! "Eglot keeps native contact handling; automatic LSP downloads require lsp-mode"))
