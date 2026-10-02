;;; local/save-format/doctor.el -*- lexical-binding: t; -*-
(unless (modulep! :editor format)
  (warn! "local/save-format expects :editor format; idle saving remains available."))
(when (and (modulep! :local environment) (not (modulep! :editor format +onsave)))
  (warn! "Enable :editor (format +onsave) for automatic project formatting."))
