;;; local/project-commands/doctor.el -*- lexical-binding: t; -*-
(unless (modulep! :local environment)
  (warn! "Project commands will use the native Emacs environment (mise integration disabled)."))
