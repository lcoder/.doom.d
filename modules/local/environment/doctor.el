;;; local/environment/doctor.el -*- lexical-binding: t; -*-

(unless (version<= "29.1" emacs-version)
  (error! "local/environment requires Emacs 29.1 or newer"))
(unless (executable-find "mise")
  (warn! "mise is absent from this machine's Doom environment; unmanaged editing remains available"))
(unless (fboundp 'file-notify-add-watch)
  (warn! "File notifications unavailable; environment changes use focus and bounded idle checks"))

