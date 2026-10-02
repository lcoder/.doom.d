# Repository Guidelines

## Project Structure & Module Organization

This repository contains shared Doom Emacs configuration for macOS, targeting Emacs 29.1+. `init.el` enables Doom modules; `packages.el` declares packages and pins; `config.el` loads local overrides and configures editor behavior. `lisp/my-dev-*.el` separates settings, environments, formatting, language support, tasks, and UI. `test/my-dev-*-test.el` contains ERT regression tests. `README.md` documents setup and daily commands; `VALIDATION.md` records verified behavior and remaining acceptance checks. `custom.el` holds Customize settings. Runtime assets and caches belong outside this repository.

## Build, Test, and Development Commands

Run Doom CLI commands from this directory using the installed Doom executable:

- `doom sync`: synchronize changes to `init.el` or `packages.el`; restart Emacs afterward.
- `doom sync --env`: synchronize packages and refresh the machine's environment cache on supported Doom versions.
- `M-x doom/reload`: reload configuration changes.
- `M-x my/dev-doctor`: inspect local tools and project capabilities.
- `M-x my/dev-setup`: explicitly install missing development dependencies.
- `git diff --check`: check whitespace before submitting.

## Coding Style & Naming Conventions

Use spaces, standard Emacs Lisp indentation (two-space body indentation), and `indent-region`. Retain lexical-binding headers in modules. Use kebab-case names, `my/dev-` for development features, and `--` for internal helpers. Add docstrings to functions and configurable variables. Use `after!` and `use-package!` for package configuration. Keep hooks, advice, timers, and keybindings safe to reload without duplication.

## Testing Guidelines

Use the running Doom Emacs server through `emacsclient` for evaluation, syntax checks, and byte compilation. Run the suite with:

```sh
emacsclient --eval '
  (progn
    (dolist (file (directory-files
                  (expand-file-name "test/" doom-user-dir) t "-test\\.el$"))
      (load file nil t))
    (ert-run-tests-batch "^my/dev-"))'
```

Never use `ert-run-tests-batch-and-exit` against the live server. Name tests `my/dev-<feature>-<behavior>` and cover changed behavior, missing-tool fallback, and reload safety. No numeric coverage threshold is defined. Record relevant manual checks and unresolved platform limitations in `VALIDATION.md`.

## Commit & Pull Request Guidelines

Follow recent history: English Conventional Commit subjects, such as `fix(doom): guard package lookup`, with concise Chinese bodies describing behavior and validation. Keep commits focused. PRs should explain the change, checks performed, relevant versions, linked issues when applicable, and screenshots for visual changes.

## Configuration & Agent Instructions

Keep machine paths and overrides in ignored `local.el`; never commit secrets, environment caches, or generated binaries. Discover tools by capability and respect project configuration. Preserve unrelated working-tree changes. Before committing or pushing, verify the remote and personal versus company GitHub identity; this checkout uses personal repository `lcoder/.doom.d`.
