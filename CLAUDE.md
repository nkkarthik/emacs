# ~/e — the Emacs fork and the fleet monorepo

This file is the entry point for any Claude session started anywhere in `~/e`.
It is deliberately SHORT and it is the only agent file in this repo that is NOT
git-crypted, so it stays readable on a machine without the key — during
bootstrap, on a fresh clone, or when something is wrong enough that the keys are
not loaded yet.

Everything here is true regardless of WHO you are. Identity lives one level
down; see "Which identity are you" below.

## What this repository is

`~/e` is BOTH a fork of GNU Emacs AND the monorepo of a six-machine fleet. It
is ours all the way down — the C in `src/`, the Elisp in `lisp/`, and the
fleet's own code in `z/` and `a/`. Changing the language the fleet is written in
is a normal, available move here, not an exotic one.

| Path | What it is |
|---|---|
| `src/` | Emacs C core. Daemon interaction guards live in `minibuf.c`, `keyboard.c`. |
| `lisp/` | Built-in Emacs Lisp. Server disconnect safeguards in `server.el`. |
| `lib-src/` | Built helpers, including THIS fork's `emacsclient` — use it, not Homebrew's. |
| `test/` | Upstream-style ERT and core regression tests. |
| `z/` | Fleet orchestration, z-server, the board, the iOS/macOS apps. git-crypted. |
| `a/` | Manager layer: agent runners, SSE transport, async helpers, per-machine config. git-crypted. |
| `x/` | Pinned third-party submodules. Initialise only the ones a machine uses. |

A C or `lisp/` change requires REBUILDING Emacs on every machine. Restarting an
old binary is not a deploy.

## Which identity are you

Read the ONE file that matches you. Do not read both — they are large, and
loading the other identity's contract wastes the context you need for work.

- **You are `z`** — the fleet orchestrator, running in book's named `z` daemon,
  working directory `~/e/z` → read `z/CLAUDE.md`.
- **You are `a`** — the manager, running in book's default daemon, working
  directory `~/e/a` → read `a/CLAUDE.md`.

These are NOT `@`-imports on purpose. `@path` in a CLAUDE.md is an
UNCONDITIONAL import: writing both here would load both contracts into every
session, which is exactly the thing this file exists to avoid.

**Codex executors get NO agent file at all.** Every instruction a codex worker
needs arrives in its task's `text` field in the task DB. That is a deliberate
design decision, and it is why a handoff must be COMPLETE and self-contained —
there is no ambient document filling in what the handoff left out.

## Rules that bind everyone

These hold for `z`, for `a`, and — because handoffs must carry them explicitly —
for every codex task too.

**1. No `message` calls in code.** `message` writes the echo area AND
`*Messages*`, and the echo-area update FORCES REDISPLAY. In a daemon nobody
reads it, so every call is pure cost. Use, in order: nothing at all (count it
instead), the durable z-server transcript, a package-owned buffer, or
`display-warning` for what a human must act on. A `message` call in a timer,
sentinel, hook, SSE callback or process filter is a DEFECT, not a style nit.
On 2026-07-25 one such call at ~6/second made an entire Emacs unusable.

**2. One Lisp thread per image.** Anything reachable from a timer, sentinel,
hook or callback must be async, or it stops that machine's fleet participation.
No synchronous process/network/TRAMP calls, no `sleep-for`/`accept-process-output`
waits, no minibuffer prompts. Use `url-retrieve`, `make-process`, `sse-connect`.

**3. Name the Emacs primitive before spawning a process.** `file-exists-p` not
`test -f`; `json-parse-buffer` not `jq`; `directory-files` not `ls`;
`secure-hash` not `sha256sum`. The fleet spans BSD and GNU userlands, and shell
code fails SILENTLY on half of it and returns a confidently wrong answer.
Reserve subprocesses for programs whose output is genuinely needed — `git`,
`xcodebuild`, `codesign`, `launchctl`, `systemctl`.

**4. Fix `~/e`, do not route around it.** If the thing obstructing you lives in
this repo — including `lisp/` and `src/` — the default is to REPAIR it, because
every machine runs this same fork and the fix then exists everywhere, forever.
A workaround is justified only when the repair is out of proportion, and then
say so explicitly rather than dressing avoidance up as a rule.

**5. One task, one directory, one commit — and deploy it.** Commit your own
work scoped to your declared paths, then make the change LIVE (reload the Elisp,
restart the daemon, rebuild the app). A change committed but not deployed reads
as done and is not; that has cost hours more than once. Verify the RUNNING
behaviour, not the file on disk.

**6. Write paths as `~/e/...`, never a hardcoded home.** Machine homes differ:
book `/Users/knannuru`, k.local `/home/knannuru`, m.local and mnk.local
`/Users/k`, ksys.local `/home/krishna`. Let the claiming daemon expand it.

**7. Report measurements, not rhetoric.** Give the number, the command, the
`file:line`, the exact error string. Phrases like "load-bearing", "the smoking
gun" or "fundamentally" sound concluded and invite agreement instead of
verification. If there is no measurement, say there is no measurement.

**8. Data lives outside the repo.** Adapters, weights, corpora, conversion
output and task scratch go under `~/z`; settings and caches under the XDG dirs;
durable curated knowledge in `~/v/r` (org-roam, Syncthing-replicated, NOT a git
repo — never try to commit it). Never write generated data into `~/e`.

## The fleet, in one table

| Machine | Arch / OS | Distinguishing capability |
|---|---|---|
| `book` | arm64 / macOS | Both Claude sessions; VPN for Jira; kphone paired; dev + deploy |
| `m.local` | arm64 / macOS | M1 Ultra 128 GB. Xcode + simulators + kphone paired. Heavy mac work |
| `mnk.local` | arm64 / macOS | M4 Pro 24 GB. Xcode + simulators, NOT kphone-paired |
| `k.local` | x86_64 / Linux | GPU/CUDA, training, vLLM, hosts z-server, NFS `~/k` |
| `ksys.local` | x86_64 / Linux | Always-on laptop (sleep masked, runs lid-closed). Small-Linux rung |
| `i.local` | x86_64 / macOS | The ONLY Intel Mac. C toolchain only — no Xcode, never iOS work |

Capabilities and roles are LIVE DATA, not prose: ask `GET /fleet` on
`k.local:7700`. This table orients you; it does not decide routing.

Risky core changes roll out `mnk -> m -> ksys -> k -> book`, because a bad
binary on k.local takes z-server with it and on book takes both Claude sessions.
