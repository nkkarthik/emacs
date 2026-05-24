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
- `launch` / `launchr` — install + (re)load macOS LaunchAgent plists from `a/daemon/e.plist` / `a/daemon/ec.plist`
- `system` — install + enable the Linux systemd user unit from `a/daemon/e.service`

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
- `init.el`, `early-init.el` — personal Emacs config, symlinked into `~/.emacs.d/` by `make -f e.mk userdir`
- `compose.yaml`, `Dockerfile` — personal containerized build
- `a/` — personal elisp packages directory (user's chosen name in place of the conventional `site-lisp/`); add new personal packages here as `a/<package>/<file>.el`. See `a/CLAUDE.md` for package-level conventions.
- `a/daemon/e.plist`, `a/daemon/ec.plist` — macOS LaunchAgent definitions for `emacs --daemon` and `emacsclient`; copied into `~/Library/LaunchAgents/` by `make -f e.mk launch`. `a/daemon/` is excluded from the `install-a` rsync so it does not land in the bundle's `site-lisp/`.
- `a/daemon/e.service` — Linux systemd user unit for the Emacs daemon; installed by `make -f e.mk system`.
- `widget-test.el`, `z` — personal scratch

## Working log in `CLAUDE.org`

Read `* claude` heading in `e.org` as you work in this repo for your goals to work to until reached. Pick the next task in the section, finish it and mark done as DONE in `CLAUDE.org` file under `* done` section. iff the task belongs to any sub agents under a/, it will mentioned to add to a file under that file in a, in that case just add that to `* claude` section in that file and move on to next task.

When `* claude` in `e.org` is empty after finishing a task, stop and wait silently — do not auto-suggest the next task or ask "what's next". The user drives the queue by editing `e.org`; resume only when they add a new task there or tell you to.

While idle on an empty queue, run a background watcher on `e.org` so the next edit re-wakes the session automatically. The watcher has two jobs that both need to run on a loop, since edits may come from either side:

- **Local edits:** poll `stat -f %m e.org` every few seconds and emit when the file's mtime advances.
- **Remote edits:** every ~60 seconds, if the working tree is clean, run `git pull --rebase` (or equivalent fetch+ff-only) to pull in any edits pushed from another machine. After a successful pull, re-read `e.org` and emit if the `* claude` queue gained a task.

On each wake:
1. If the working tree is clean and a pull wasn't just performed, run `git pull --rebase`.
2. Re-read `* claude` in `e.org`. If a task appeared, pick it up. Otherwise stay silent (the change was unrelated).

- When you start a task that will produce a tangible change (file edit, build wiring, verification, etc.), add a one-line entry under `** todo` first in the CLAUDE.org.
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
