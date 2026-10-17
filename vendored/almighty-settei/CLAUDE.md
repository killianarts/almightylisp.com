# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

`almighty-settei` is a Common Lisp settings library for web applications. Settings are declared with `define-setting` (a keyword name `module/name`, a kind class, documentation, a default, environment variables), read with `(setting name)`, and loaded from their sources once, when the program starts, with `load-settings`. README.org is the user documentation.

**Dependencies**: none beyond ASDF/UIOP.
**Test framework**: `lisp-unit2` (from Quicklisp).

## Build & Test Commands

```lisp
(asdf:load-system :almighty-settei)
(asdf:test-system :almighty-settei)
```

## Architecture

One package per file, re-exported by the umbrella package `almighty-settei` (nickname `settei`) in `src/package.lisp`, loaded last.

| File | Package | Purpose |
|------|---------|---------|
| `conditions.lisp` | `almighty-settei/conditions` | Conditions, `problem` |
| `kinds.lisp` | `almighty-settei/kinds` | `setting` and its kinds; `coerce-value`, `check-value`, `accept-value`, `render-value` |
| `registry.lisp` | `almighty-settei/registry` | `registry`, `*registry*`, `define-setting`, `find-setting`, derived environment variable names |
| `dotenv.lisp` | `almighty-settei/dotenv` | `.env` parsing |
| `configuration.lisp` | `almighty-settei/configuration` | `configuration`, the sources in order, `setting` and its `setf`, `load-settings`, `configure`, `per-environment`, `with-settings`, listeners, the image-restore hook, `setting`'s compiler macro |
| `report.lisp` | `almighty-settei/report` | `settings-report`, `print-settings`, `redact` |

### Key Design Decisions

- **The registry holds everything kept between runs**: settings, the project's values (`configure`), listeners. Tests bind `*registry*` to `(make-registry)` to isolate themselves.
- **A configuration is one run's values**, each `(value . source)`, read lazily and cached. `load-settings` reads all of them to check them. The program's configuration is the global value of `*configuration*`; `with-settings` binds a copy in one thread.
- **Sources, in order**: `:session`, `:environment`, `:dotenv`, `:project`, `:environment-default`, `:computed`, `:default`.
- **Nothing reads the environment at load time.** A saved image forgets its configuration on restore and defaults to `:production` (`forget-on-restore`).
- **Values computed from others** (`:environment-default`, `:computed`) are forgotten whenever any setting is set, so they are computed again.
- **Errors are collected**: `load-settings` signals one `settings-invalid` with every problem.
- **`configure` is a macro** (for its grouped form, `(:server :port 5000)`); `add-project-values` is the function behind it. Configured names are checked by `load-settings`, not when configured, so a project's settings file may load before the modules that define the settings.
- **`setting` is both the base class and the reader function** — the same symbol.
- **Frameworks hide the library.** Shiso re-exports the user-facing API and renames the environment setting with `define-environment-setting` (`*environment-setting*` holds its name; never write `:settei/environment` in library code). User-visible text should not name the library.
