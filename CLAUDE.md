# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this repo is

A personal fork of GNU Emacs (currently 32.0.50). `origin` is `github.com/nkkarthik/emacs`, which periodically merges from `emacs-mirror/emacs` upstream. Most files in this tree are upstream Emacs source; a small set of root-level files and the `a/` directory are personal additions (see below). Treat upstream paths (`src/`, `lisp/`, `lib/`, `lib-src/`, `lwlib/`, `oldXMenu/`, `modules/`, `nt/`, `nextstep/`, `etc/`, `info/`, `doc/`, `admin/`, `build-aux/`, `m4/`, `java/`, `cross/`, `test/`, `leim/`, `msdos/`) as upstream — edits there will conflict on the next `emacs-mirror` merge.

## Build — use `e.mk`, not plain `make`

The primary build entry point is `e.mk` at the repo root (personal wrapper). Plain `./configure && make` works but isn't what the user runs.

```
make -f e.mk           # platform-detected: Darwin or Linux
make -f e.mk Darwin    # macOS: deps + configure + build + userdir symlinks
make -f e.mk Linux     # Ubuntu GTK build
make -f e.mk kws       # Fedora/WSL build (also builds tree-sitter from source)
```

What `e.mk` does on macOS:
- Installs build deps via Homebrew (`autoconf`, `texinfo`, `gnutls`, `tree-sitter`, `libgccjit`, etc.)
- Runs `./autogen.sh` then `./configure --with-ns --with-modules --with-json --with-tree-sitter --with-sqlite3 --prefix=$HOME/.local/emacs`
- `make` then `make install` → produces `nextstep/Emacs.app/`
- `userdir` symlinks `init.el` and `early-init.el` from this repo into `~/.emacs.d/`

Other useful `e.mk` targets:
- `brew-bin` / `local-bin` — symlink `src/emacs` and `lib-src/emacsclient` into `/opt/homebrew/bin` or `/usr/local/bin`
- `launch` / `launchr` — install + (re)load macOS LaunchAgent plists from `e.plist` / `ec.plist`
- `system` — install + enable the Linux systemd user unit from `e.service`

Variables: `EMACS_PREFIX` (default `$HOME/.local/emacs`), `JOBS` (default detected).

Sanity check after a build:
```
src/emacs --batch --eval '(message system-configuration-features)'
```

## Tests

Standard upstream `test/` tree runs via `make check`. Individual ERT tests:
```
make -C test FILE.log         # run tests in test/FILE.el (or test/lisp/FILE-tests.el)
```
See `test/README` for selector syntax.

## Personal vs. upstream files

Personal files at repo root (do not assume these exist upstream):
- `e.mk` — build wrapper described above
- `e.org` — quick-reference build/launch notes
- `e.plist`, `ec.plist` — macOS LaunchAgent definitions for `emacs --daemon` and `emacsclient`
- `e.service` — Linux systemd user unit for the Emacs daemon
- `init.el`, `early-init.el` — personal Emacs config, symlinked into `~/.emacs.d/` by `make -f e.mk userdir`
- `compose.yaml`, `Dockerfile` — personal containerized build
- `a/` — personal elisp packages directory (user's chosen name in place of the conventional `site-lisp/`); add new personal packages here as `a/<package>/<file>.el`. See `a/CLAUDE.md` for package-level conventions.
- `widget-test.el`, `z` — personal scratch

## Working log in `e.org`

Maintain a `* claude` heading in `e.org` with two subheadings, `** todo` and `** done`. As you work in this repo:

- When you start a task that will produce a tangible change (file edit, build wiring, verification, etc.), add a one-line entry under `** todo` first.
- When you finish, move the line to `** done` and prefix it with the date (`YYYY-MM-DD`).
- For small, immediately-completed work you can skip the todo step and write straight into `** done`.

Keep entries terse — one line each, oldest at the top so it reads chronologically. The log is for future sessions to scan quickly; it is not a substitute for the git history.

## Git workflow

Push URL is SSH (`git@github.com:nkkarthik/emacs.git`), fetch is HTTPS. Always run `git pull --rebase` before `git push` — never bare push.

This fork tracks `emacs-mirror/emacs`. Merge commits like `Merge branch 'emacs-mirror:master' into master` and `Merge from origin/emacs-31` are routine. When working in upstream paths, expect history to be dominated by upstream commits — `git log -- <path>` is more useful than session-start `git log`.

## Install layout (macOS, what shows up in `Emacs.app`)

`make install` produces `nextstep/Emacs.app/Contents/Resources/`:
- `lisp/` — preloaded Emacs Lisp (copied from `${srcdir}/lisp/`)
- `etc/` — data files
- `site-lisp/` — **created empty** with a synthesized `subdirs.el` (from `Makefile.in:605-613` `write_subdir`). The synthesized `subdirs.el` is only written if one doesn't already exist, so a user-supplied version is preserved.
- The Makefile's `COPYDIR = ${srcdir}/etc ${srcdir}/lisp` (`Makefile.in:352`) — so the top-level `a/` directory is **not** auto-copied to the install destination. Wiring `a/` into the installed app requires either a post-install rsync target in `e.mk` (e.g. `rsync -a $(srcdir)/a/ $(ns_appresdir)/site-lisp/`) or a `Makefile.in` patch.
