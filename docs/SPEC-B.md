# Aura B — TUI Terminal Emulator: Settled Design Spec

Date: 2026-08-25 · Status: approved by user, ready to build
Supersedes: MVP line-based CLI (kept as `aura --cli` for debugging)

## Goal

Turn Aura from a line-based shell pipe into a **real terminal emulator**,
delivered in two phases:

- **Phase B (now):** full-screen TUI terminal inside the host console.
  Proves the emulator core with zero GUI dependencies.
- **Phase A (next):** GTK3 GUI wrapping the *same* core (GtkNotebook tabs,
  custom-drawn grid). No core rewrite expected.

## Architecture (evolve in place, git-init first)

Reused unchanged:
- `src/aura_ansi.f90` — VT parser → cell grid (+ alt-screen support to ADD)
- `src/aura_session.f90` — scrollback ring + AI context extraction
- `src/aura_pty.f90` + `csrc/aura_pty_bridge.c` — ConPTY / forkpty
- `src/aura_llm.f90` — AI interface, rule fallback
- `src/aura_config.f90`, `fpm.toml`, `build.sh`

New:
- `src/aura_keys.f90` — raw keyboard via Win32 ReadConsoleInput / Unix termios;
  decodes key events, routes reserved keys vs pass-through
- `src/aura_render.f90` — diff renderer (grid→console writes), scroll-mode
  painter, AI drawer overlay, status-line tab bar; full repaint on session switch
- `src/aura_http.f90` — minimal HTTP/1.1 POST client over WinSock2/POSIX
  socket C bridge (`csrc/aura_net.c`) for Ollama OpenAI-compatible endpoint

Rewritten:
- `app/main.f90` — raw console event loop (drain PTYs → update grids →
  diff-render active session → process keys)

## Behavior decisions (all user-approved)

| Area | Decision |
|---|---|
| Input | Raw pass-through; child shell owns editing/history/Tab/Ctrl+C |
| Reserved keys | Ctrl+T new session · Ctrl+W close · Ctrl+Tab cycle · Ctrl+A AI drawer · Shift+PgUp/PgDn scroll mode |
| Rendering | True grid emulation, diff repaints to host console |
| Scrollback | Aura-owned ring, default 10,000 lines (`history_lines` config); Shift+PgUp/PgDn enters scroll mode, any key exits |
| Sessions | N live sessions; full pump (all PTYs drain continuously, only active renders); status-line tab bar at screen bottom; closing last exits |
| AI drawer | Bottom ~6-line overlay; question + last-N-lines context + cwd → LLM; answer rendered in drawer; **Ctrl+E** insert suggested command at prompt (edit-only), **Ctrl+Enter** execute immediately; Esc closes, grid repaints |
| LLM backend | Ollama gpt-oss:20b @ http://localhost:11434/v1 (OpenAI-compatible); llama.cpp/small-model profile later; rule-based aura-assist auto-fallback when unreachable |
| Alt screen | Implemented so cls/more behave; vim-class apps may be imperfect |

## Acceptance contract (all must pass)

1. Streaming render — `ping -t` ticks live
2. Raw keys — arrows=history, Tab completes, Ctrl+C aborts, backspace edits mid-line
3. Full-screen — `cls` correct; `more` sane (alt-screen)
4. Scroll — Shift+PgUp ≥1000 prior lines; any key returns live
5. Sessions — Ctrl+T second shell; background job accumulates while hidden; Ctrl+W closes; last close exits
6. AI drawer — real-history context → Ollama answers → Ctrl+E inserts → Esc restores grid
7. Fallback — Ollama killed → rule-based responds, no crash

## Non-goals for B

- Tabs-as-notebook GUI, mouse support, ligatures/themes engine, reflow-on-resize
  (resize handled as full repaint), Windows <10 1809 (no ConPTY), winpty shim.

## Phase A preview (not started)

gtk-fortran window; notebook = sessions array (exists); text view consumes the
same cell grid; same reserved-key semantics mapped to accelerators; AI drawer
becomes bottom sidebar. Core untouched.
