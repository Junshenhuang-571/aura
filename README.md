# Aura — Fortran AI Terminal (MVP)

A cross-platform (Windows/macOS/Linux) AI-enhanced terminal emulator written in
modern Fortran. Native terminal emulation with PTY management, a built-in
offline local AI assistant, and near-zero external runtime dependencies.

## Status

| Feature | State |
|---|---|
| ANSI/VT escape parser (SGR colours, cursor, clear, scroll) | ✅ done, unit-tested |
| PTY: ConPTY backend (Windows 10 1809+) | ✅ done, e2e-verified |
| PTY: forkpty backend (Linux/macOS) | ✅ implemented (needs Unix host to verify) |
| Session layer: scrollback + context capture | ✅ done, unit-tested |
| Local AI assistant (`aura-assist`, fully offline) | ✅ done |
| Native LLM inference hook (llm.f90 / llama.cpp slot) | 🔌 interface ready, stub linked |
| JSON config (`~/.config/aura/config.json`) | ✅ done (json-fortran-compatible format) |
| GTK3 GUI (gtk-fortran) | 🧩 skeleton in `src/aura_gui.f90` (see below) |
| fpm build | `fpm.toml` included; direct gfortran build also provided |

## Quick start

```sh
./build.sh                 # builds with gfortran + gcc, runs nothing else
./build/aura_test          # unit tests
./build/e2e_final          # end-to-end PTY round-trip test
./build/aura               # interactive terminal (CLI mode)
./build/aura --ask "list my files"     # one-shot AI query
./build/aura --config                  # write default config
```

Requirements: gfortran 11+ and any C compiler (gcc/clang/msvc-cl). No other
dependencies at runtime.

### Building with fpm

`fpm.toml` is set up for `fpm build && fpm run`. If you don't have fpm,
`build.sh` compiles the same sources directly.

## Usage

- Type shell commands normally; they go to the PTY.
- `/ai <question>` asks the assistant. It receives the last ~10 lines of real
  terminal output plus the working directory as context.
- `/exit` quits.
- One-shot mode: `aura --ask "check disk space"`.

### TUI design language

Aura uses a focused, terminal-native layout for computational physics work:

- **Midnight canvas + ice-blue data accents** keep long solver sessions easy on
  the eyes while making active controls obvious.
- The bottom status rail identifies the current workspace as
  `FORTRAN PHYSICS`, keeps shell tabs visible, and reports live versus
  scrollback mode without covering simulation output.
- `Ctrl+Shift+A` opens the **Computational Assist** drawer. Its prompt is the
  dedicated target for typed or speech-to-text input, so transcribed questions
  can be reviewed before they are sent to the model.
- The status rail exposes `VOICE READY` as a persistent input affordance;
  shell input remains untouched while the assistant drawer is closed.

Key controls: `Ctrl+T` new shell tab, `Ctrl+R` switch workspace, `Ctrl+W`
workspace picker, `Ctrl+Shift+A` assistant, and `PgUp/PgDn` scrollback.

## Configuration

`~/.config/aura/config.json` (auto-created on first `--config`):

```json
{
  "shell": "/bin/bash",
  "model": "models/stories15M.bin",
  "theme": "dark",
  "provider": "local",
  "history_lines": 1000
}
```

## Architecture

```
app/main.f90            program entry; CLI terminal loop, --ask, --config
src/aura_ansi.f90       ANSI/VT parser -> cell grid (char + fg/bg/bold/reverse)
src/aura_session.f90    session = PTY handle + scrollback + screen state
src/aura_pty.f90        Fortran facade over the C PTY ABI
csrc/aura_pty_bridge.c  Windows: ConPTY | Unix: forkpty()
src/aura_llm.f90        AI interface; prefers native backend, falls back to
                        offline rule-based 'aura-assist'
csrc/aura_llm_stub.c    no-op native backend (replace to plug llm.f90)
src/aura_gui.f90        gtk-fortran GUI skeleton (notebook tabs + text view plan)
test/                   unit tests + e2e test
```

## Plugging in a real local LLM

The MVP ships with an offline rule-based responder so the AI feature works with
zero downloads. To embed actual model weights (e.g. Karpathy's `stories15M.bin`
via the llama2.f90 port, or GGUF via llama.cpp):

1. Implement `int aura_llm_available(void)` returning 1, and
   `const char* aura_llm_generate(const char* prompt, const char* ctx)`
   in C/Fortran-with-bind(C).
2. Remove `csrc/aura_llm_stub.c` from the build (duplicate symbols otherwise).
3. Set `"model"` in the config to your weights path.

`src/aura_llm.f90` automatically prefers the native backend when present.

## Windows support notes

Windows uses **ConPTY** (`CreatePseudoConsole`, Windows 10 1809+) resolved
dynamically from kernel32 — no winpty shim, no extra DLLs. Two hard-won details
are encoded in `csrc/aura_pty_bridge.c`:

- The child process MUST be created with `STARTF_USESTDHANDLES`; otherwise its
  stdio never attaches to the pseudoconsole and writes are silently dropped.
- The pseudoconsole's child-side pipe ends must stay open until after
  `CreateProcess`, then be closed by the parent.

Default shell comes from `%COMSPEC%`. Verified end-to-end on Windows 10/11
(see `test/e2e_final.f90`).

## GTK GUI

`src/aura_gui.f90` contains the gtk-fortran binding subset and module scaffold.
Full wiring (GtkNotebook tabs, GtkTextView per tab, Ctrl+A dialog,
idle-loop drain via `g_timeout_add`) requires linking against gtk-fortran:

```sh
fpm build --flag "$(pkg-config --cflags gtk+-3.0-fortran)" \
          --link-flag "$(pkg-config --libs gtk+-3.0-fortran)"
```

On hosts without GTK (e.g. this Windows box), `aura --gui` prints a notice and
falls back to the CLI terminal, which exercises the identical session layer.
