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
| SSH workspaces (system `ssh`) | ✅ saved remote workspace targets |
| Speech-to-text | 🔌 external recorder + `transcribe-cli` adapter |
| Desktop workbench GUI | ✅ Tkinter adapter with workflow/result controls |
| Physics workbench manifest | ✅ templates, MPI/OpenMP, SLURM/local, sweeps, run tracking, result adapters |
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

## Computational physics workbench

The repository includes `aura-workbench.toml`, a dependency-free project
manifest used by the desktop workbench GUI. It
keeps project metadata, Fortran templates, compiler flags, MPI/OpenMP choices,
external build/run/test commands, local or SLURM execution, parameter sweeps,
run tracking, and result adapters for visualization, convergence checks, and
HDF5/NetCDF browsing in one inspectable file. Aura does **not** embed GTK,
compilers, MPI, SLURM, HDF5/NetCDF, or a plotting library; commands are
adapters to tools already installed on the host.

The same manifest can be inspected or executed from the CLI, or opened in the
desktop GUI:

```sh
./build/aura --workbench aura-workbench.toml summary
./build/aura --workbench aura-workbench.toml preview
./build/aura --workbench aura-workbench.toml sweep-plan
./build/aura --workbench aura-workbench.toml build
./build/aura --workbench aura-workbench.toml run
./build/aura --workbench aura-workbench.toml test
./build/aura --workbench aura-workbench.toml monitor run-001
./build/aura --gui
```

The manifest uses small TOML sections (`project`, `toolchain`, `mpi`,
`openmp`, `scheduler`, `commands`, `tracking`, `results`, `templates.*`, and
`sweep.*`). The `[results]` section can point to Python/matplotlib, ParaView,
HDF5, or NetCDF commands. `scheduler.kind = "slurm"` wraps the run command in
`sbatch`; `local` runs it directly.
Sweep values form a Cartesian product and are exposed to the GUI as named
points.  `{mpi_prefix}`, `{omp_prefix}`, `{source_dir}`, `{main}`, and other
documented substitutions are expanded before an external command is run.
Run status is recorded under `tracking.directory/<run-id>/` as
`status.toml`, allowing a monitor panel to remain useful even when the solver
is not running. `preview` (also accepted as `dry-run`) prints resolved build,
run, and test commands without executing them. `sweep-plan` prints deterministic
`sweep-N` IDs, parameter values, and resolved run commands without creating
tracking directories.

### Workbench workflow and limitations

1. Copy the sample manifest and select a template/compiler for the solver.
2. Validate the command with `summary`, then build and run locally.
3. Enable MPI/OpenMP or SLURM only after checking the resulting command.
4. Give each sweep point a run ID and write solver output into its tracked
   directory for later visualization.

The GUI runs build/run/test and configured result commands in a background
thread, streams output into its run panel, lists sweep dimensions, and exposes
MPI/OpenMP/SLURM settings. Configure `[results]` commands to connect
matplotlib, ParaView, HDF5, or NetCDF tooling.

This is a foundation, not a job database: the current CLI does not launch
multiple sweep points automatically, stream scheduler logs, or parse result
formats natively. The GUI intentionally uses external adapters rather than hard-linking large
scientific libraries; the parser supports the manifest subset above rather
than all TOML.
MPI, OpenMP, SLURM, and GUI availability depend on external installations.

## Usage

- Type shell commands normally; they go to the PTY.
- `/ai <question>` asks the assistant. It receives the last ~10 lines of real
  terminal output plus the working directory as context.
- `/exit` quits.
- One-shot mode: `aura --ask "check disk space"`.

### SSH workspaces

Aura can persist remote workspaces and launches the platform's `ssh` command
inside the same PTY/session layer. In the workspace picker, enter a remote
workspace as:

```text
name|user@host|22|/path/to/project
```

The workspace is saved in `workspaces.json`, reconnects using your normal SSH
configuration/agent, and sends `cd` to the requested remote directory after
connection. Aura does not handle passwords or private keys itself.

### Speech-to-text adapter

Speech input is intentionally external and local. Configure a recorder and a
transcriber command in `config.json`; both use `{wav}` as the output/input
placeholder:

```json
{
  "stt_record_command": "ffmpeg -y -f dshow -i audio=\"Microphone\" -t 8 -ar 16000 -ac 1 {wav}",
  "stt_transcribe_command": "transcribe-cli -m models/whisper-small.gguf {wav}"
}
```

Build `handy-computer/transcribe.cpp` and download a compatible GGUF model
before using `Ctrl+Shift+V`. Aura records to `aura_voice.wav`, captures the
adapter's stdout, and opens the transcript in the AI drawer for review before
submission. The transcript is never sent directly to the shell.

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
workspace picker, `Ctrl+Shift+A` assistant, `Ctrl+Shift+V` voice capture,
and `PgUp/PgDn` scrollback.

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
src/aura_workbench.f90  manifest, command adapters, sweeps, run tracking
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

## Desktop workbench GUI

`aura --gui` launches `tools/aura_workbench_gui.py` with Python's Tkinter
desktop toolkit. Python 3.11+ is recommended for built-in `tomllib`; the
scientific result tools remain configurable commands, so users can select
their preferred matplotlib, ParaView, HDF5, or NetCDF stack.
