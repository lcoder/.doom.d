;;; local/project-commands/config.el -*- lexical-binding: t; -*-
(load! "+discovery")
(load! "+commands")
(add-hook 'hack-local-variables-hook #'+local-project-prepare-h 90)
(add-hook 'find-file-hook #'+local-project-prepare-h 90)
(add-hook 'after-save-hook #'+local-project-manifest-saved-h)
(when (modulep! :local environment)
  (add-hook '+local-env-changed-hook #'+local-project-env-changed-h))
(after! compile
  (advice-add 'compile :around #'+local-project-compile-a)
  (advice-add 'recompile :around #'+local-project-recompile-a))
(after! projectile
  (advice-add 'projectile-compile-project :around #'+local-project-build-a)
  (advice-add 'projectile-test-project :around #'+local-project-test-a)
  (advice-add 'projectile-run-project :around #'+local-project-run-a)
  (advice-add 'projectile-repeat-last-command :around #'+local-project-repeat-a))
