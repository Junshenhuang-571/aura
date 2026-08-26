/* aura_console_win.c — raw console input/output bridge (Windows)
 *
 * Input:  ReadConsoleInputW -> KEY_EVENT records -> compact event structs
 * Output: WriteConsoleOutputCharacterW / SetConsoleCursorPosition etc. for
 *         the diff renderer.
 */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <stdio.h>
#include <io.h>
#include <stdarg.h>

/* forward declarations (defined later in this file) */
static int vt_query_size(void);
static int vt_check_resize(void);

/* --- input events ------------------------------------------------------ */
/* ev_type: 0 = none/timeout, 1 = key char, 2 = key special, 3 = resize */
/* special key ids (ev_key): 1=Up 2=Down 3=Left 4=Right 5=Home 6=End
   7=PgUp 8=PgDn 9=Del 10=Ins 11=F1..: 11+F(1-12) */
typedef struct {
    int ev_type;
    wchar_t ch;        /* key char (type 1) */
    int ev_key;        /* special id (type 2) */
    int ctrl;          /* ctrl held */
    int shift;         /* shift held */
    int alt;           /* alt held */
} AuraKeyEv;

static HANDLE hConIn = NULL, hConOut = NULL;
static DWORD oldInMode = 0, oldOutMode = 0;
static int g_vt_mode = -1;   /* -1 = undecided, 1 = VT-to-stdout (pseudoconsole/ConPTY), 0 = real console */
static int g_sz_cols = 0, g_sz_rows = 0, g_sz_ok = 0;  /* cached terminal size */

/* Return 1 if we should emit raw VT to stdout. We are in a pseudo-console
   (Windows Terminal / PowerShell / ConPTY) when our stdout is a PIPE — that is
   the reliable signal. Fall back to the legacy real-console path only when
   stdout is a character device AND there is a real console window. */
static int is_vt_mode(void) {
    if (g_vt_mode < 0) {
        DWORD ft = GetFileType(GetStdHandle(STD_OUTPUT_HANDLE));
        if (ft == FILE_TYPE_PIPE) {
            g_vt_mode = 1;            /* ConPTY: stdout is a pipe -> VT to stdout */
        } else if (GetConsoleWindow() == NULL) {
            g_vt_mode = (ft == FILE_TYPE_CHAR) ? 0 : 1;
        } else {
            g_vt_mode = 0;            /* classic attached console */
        }
    }
    return g_vt_mode;
}

/* Optional debug logger, enabled when env AURA_DEBUG=1 is set.
   Writes key facts to aura_debug.log so we can diagnose the real terminal. */
static void aura_dbg(const char* fmt, ...) {
    static int enabled = -1;
    if (enabled < 0) enabled = (getenv("AURA_DEBUG") != NULL) ? 1 : 0;
    if (!enabled) return;
    FILE* f = fopen("C:/Users/junsh/projects/aura/aura_debug.log", "a");
    if (!f) return;
    va_list ap; va_start(ap, fmt); vfprintf(f, fmt, ap); va_end(ap);
    fputc('\n', f); fclose(f);
}

static void ensure_handles(void) {
    if (!hConIn) {
        /* On a real console, use the std handles directly.
           On a ConPTY/pseudoconsole (no console window) we must NOT attach to a
           parent console — the std handles are already the pseudoconsole pipes. */
        hConIn = GetStdHandle(STD_INPUT_HANDLE);
        hConOut = GetStdHandle(STD_OUTPUT_HANDLE);
        if (is_vt_mode()) {
            /* Pseudoconsole: enables VT processing on the output pipe so our
               ESC sequences render; leave input alone (we read stdin escapes). */
            DWORD m = 0;
            if (GetConsoleMode(hConOut, &m)) {
                SetConsoleMode(hConOut, m | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
            }
        } else {
            GetConsoleMode(hConIn, &oldInMode);
            GetConsoleMode(hConOut, &oldOutMode);
        }
    }
}

void aura_con_raw_enter(void)
{
    DWORD mode;
    ensure_handles();
    aura_dbg("raw_enter: vt_mode=%d GetConsoleWindow=%p outFileType=%d",
             is_vt_mode(), (void*)GetConsoleWindow(),
             (int)GetFileType(GetStdHandle(STD_OUTPUT_HANDLE)));
    /* Force UTF-8 so the box/diamond glyphs we emit as UTF-8 render correctly
       instead of as CP1252 mojibake (e.g. '─' -> 'ΓöÇ'). */
    SetConsoleOutputCP(CP_UTF8);
    SetConsoleCP(CP_UTF8);
    /* Always enable VT processing on the output handle so our ESC sequences
       (cursor moves, SGR colors, show/hide cursor) render in every host. */
    {
        DWORD m = 0;
        if (GetConsoleMode(hConOut, &m))
            SetConsoleMode(hConOut, m | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
    }
    if (is_vt_mode()) {
        /* Pseudoconsole: stdin is a pipe (no console mode to set). */
        return;
    }
    mode = oldInMode;
    mode &= ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
    mode |= ENABLE_WINDOW_INPUT;              /* want resize events */
    SetConsoleMode(hConIn, mode);
}

void aura_con_raw_exit(void)
{
    if (is_vt_mode()) return;   /* nothing to restore on a pseudoconsole */
    if (hConIn) { SetConsoleMode(hConIn, oldInMode); }
    if (hConOut) { SetConsoleMode(hConOut, oldOutMode); }
}

/* Poll one key event. Returns 0 if none available within wait_ms. */
int aura_con_poll_key(int wait_ms, AuraKeyEv* out)
{
    INPUT_RECORD rec;
    DWORD n = 0;
    ULONGLONG deadline = GetTickCount64() + (ULONGLONG)wait_ms;

    ensure_handles();
    out->ev_type = 0;

    /* --- ConPTY / pseudoconsole path: read bytes from the stdin pipe --- */
    if (is_vt_mode()) {
        /* Resize events arrive on the console input buffer, not the pipe. */
        if (vt_check_resize()) {
            out->ev_type = 3;   /* KEV_RESIZE */
            return 1;
        }
        unsigned char buf[16];
        int rdn, i;
        int tot = 0;
        char seq[16];
        HANDLE hin = GetStdHandle(STD_INPUT_HANDLE);
        if (WaitForSingleObject(hin, (DWORD)wait_ms) != WAIT_OBJECT_0) return 0;
        rdn = (int)read(STDIN_FILENO, buf, sizeof(buf));
        if (rdn <= 0) return 0;
        /* buffer what we got for escape decoding */
        for (i = 0; i < rdn && tot < 15; i++) seq[tot++] = (char)buf[i];
        seq[tot] = 0;
        if (seq[0] == '\x1b') {
            if (tot >= 3 && seq[1] == '[') {
                out->ev_type = 2;
                switch (seq[2]) {
                    case 'A': out->ev_key = 1; break;   /* up */
                    case 'B': out->ev_key = 2; break;   /* down */
                    case 'C': out->ev_key = 4; break;   /* right */
                    case 'D': out->ev_key = 3; break;   /* left */
                    case 'H': out->ev_key = 5; break;   /* home */
                    case 'F': out->ev_key = 6; break;   /* end */
                    case '5': out->ev_key = 7; break;   /* pgup */
                    case '6': out->ev_key = 8; break;   /* pgdn */
                    case '3': out->ev_key = 9; break;   /* del */
                    default: out->ev_type = 0; break;
                }
                if (out->ev_type) {
                    aura_dbg("key: vt path type=%d ch=%u key=%d ctrl=%d shift=%d seq='%s'",
                             out->ev_type, out->ch, out->ev_key, out->ctrl, out->shift, seq);
                    return 1;
                }
            }
            /* lone ESC (e.g. to close overlay) */
            if (tot == 1) { out->ev_type = 1; out->ch = 27; aura_dbg("key: vt lone-ESC"); return 1; }
            return 0;
        }
        /* Ctrl+letter: bare control char */
        if (seq[0] >= 1 && seq[0] <= 26) {
            out->ev_type = 1; out->ch = (unsigned)(seq[0] + 'a' - 1); out->ctrl = 1;
            aura_dbg("key: vt ctrl-letter ch=%u (%s)", out->ch, seq);
            return 1;
        }
        if (seq[0] == '\r' || seq[0] == '\n') { out->ev_type = 1; out->ch = (unsigned)seq[0]; aura_dbg("key: vt CR/LF"); return 1; }
        if (seq[0] == '\t') { out->ev_type = 1; out->ch = 9; aura_dbg("key: vt TAB"); return 1; }
        /* printable UTF-8: decode first codepoint */
        out->ev_type = 1;
        out->ch = (unsigned)buf[0];   /* single byte; multibyte handled well enough */
        if (rdn > 1) {
            /* accumulate a UTF-8 codepoint (1-4 bytes) into one event */
            unsigned cp = 0; int bytes = 1;
            if ((buf[0] & 0xE0) == 0xC0) bytes = 2;
            else if ((buf[0] & 0xF0) == 0xE0) bytes = 3;
            else if ((buf[0] & 0xF8) == 0xF0) bytes = 4;
            if (bytes <= rdn) {
                int k; unsigned tmp = buf[0];
                if (bytes == 2) tmp &= 0x1F;
                else if (bytes == 3) tmp &= 0x0F;
                else if (bytes == 4) tmp &= 0x07;
                cp = tmp;
                for (k = 1; k < bytes; k++) cp = (cp << 6) | (buf[k] & 0x3F);
                out->ch = cp;
            }
        }
        return 1;
    }

    /* --- real console path: ReadConsoleInputW --- */
    for (;;) {
        if (!GetNumberOfConsoleInputEvents(hConIn, &n)) return 0;
        if (n == 0) {
            if ((int)(GetTickCount64() - deadline) >= 0) return 0;
            Sleep(10);
            continue;
        }
        if (!ReadConsoleInputW(hConIn, &rec, 1, &n) || n == 0) return 0;
        if (rec.EventType == WINDOW_BUFFER_SIZE_EVENT) {
            out->ev_type = 3;
            return 1;
        }
        if (rec.EventType != KEY_EVENT || !rec.Event.KeyEvent.bKeyDown) continue;
        {
            WORD vk = rec.Event.KeyEvent.wVirtualKeyCode;
            WCHAR uch = rec.Event.KeyEvent.uChar.UnicodeChar;
            DWORD mods = rec.Event.KeyEvent.dwControlKeyState;
            int is_ctrl  = (mods & LEFT_CTRL_PRESSED) || (mods & RIGHT_CTRL_PRESSED);
            int is_shift = (mods & SHIFT_PRESSED) != 0;
            int is_alt   = (mods & LEFT_ALT_PRESSED) || (mods & RIGHT_ALT_PRESSED);
            out->ctrl = is_ctrl; out->shift = is_shift; out->alt = is_alt;

            /* Ctrl+letter => control character in uChar; report as char with ctrl flag */
            if (uch != 0) {
                out->ev_type = 1;
                out->ch = uch;
                aura_dbg("key: console path type=1 ch=%u (%c) ctrl=%d shift=%d alt=%d vk=%u",
                         uch, (uch>=32&&uch<127)?(char)uch:'.', is_ctrl, is_shift, is_alt, (unsigned)vk);
                return 1;
            }
            /* no unicode char: map special keys */
            switch (vk) {
            case VK_UP:    out->ev_key = 1; break;
            case VK_DOWN:  out->ev_key = 2; break;
            case VK_LEFT:  out->ev_key = 3; break;
            case VK_RIGHT: out->ev_key = 4; break;
            case VK_HOME:  out->ev_key = 5; break;
            case VK_END:   out->ev_key = 6; break;
            case VK_PRIOR: out->ev_key = 7; break;
            case VK_NEXT:  out->ev_key = 8; break;
            case VK_DELETE:out->ev_key = 9; break;
            case VK_INSERT:out->ev_key = 10;break;
            case VK_F1: case VK_F2: case VK_F3: case VK_F4:
            case VK_F5: case VK_F6: case VK_F7: case VK_F8:
            case VK_F9: case VK_F10: case VK_F11: case VK_F12:
                out->ev_key = 11 + (vk - VK_F1); break;
            default: continue;    /* ignore other non-char keys */
            }
            out->ev_type = 2;
            return 1;
        }
    }
}

/* --- output helpers ----------------------------------------------------- */

void aura_con_write_at(int col, int row, const wchar_t* text, int nchars)
{
    ensure_handles();
    /* Always emit cursor-position escape + UTF-8 text to stdout.
       On a ConPTY this is the only correct path; on a real console with VT
       processing enabled (see aura_con_raw_enter) it also renders correctly
       and lets us carry SGR color escapes produced by the Fortran renderer. */
    int r = row + 1, c = col + 1;
    char prefix[32];
    int plen = snprintf(prefix, sizeof(prefix), "\x1b[%d;%dH", r, c);
    DWORD w;
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    WriteFile(out, prefix, (DWORD)plen, &w, NULL);
    if (nchars > 0) {
        int need = WideCharToMultiByte(CP_UTF8, 0, text, nchars, NULL, 0, NULL, NULL);
        char* buf = (char*)malloc((size_t)need + 1);
        if (buf) {
            WideCharToMultiByte(CP_UTF8, 0, text, nchars, buf, need, NULL, NULL);
            WriteFile(out, buf, (DWORD)need, &w, NULL);
            free(buf);
        }
    }
}

void aura_con_get_size(int* cols, int* rows)
{
    CONSOLE_SCREEN_BUFFER_INFO info;
    ensure_handles();

    if (is_vt_mode()) {
        /* On a ConPTY the stdout handle is a PIPE, so GetConsoleScreenBufferInfo
           fails. Query the real terminal size via the VT size report (ESC[18t),
           which Windows Terminal / ConPTY answer with ESC[8;ROWS;COLS*t. */
        if (!g_sz_ok) vt_query_size();
        if (g_sz_ok) {
            *cols = g_sz_cols;
            *rows = g_sz_rows;
            return;
        }
        /* Fallback defaults if the report never arrived. */
        *cols = 80; *rows = 25;
        return;
    }

    if (GetConsoleScreenBufferInfo(hConOut, &info)) {
        *cols = (int)(info.srWindow.Right - info.srWindow.Left + 1);
        *rows = (int)(info.srWindow.Bottom - info.srWindow.Top + 1);
    } else {
        *cols = 80; *rows = 25;
    }
}

/* Query terminal size via VT report; cache result in g_sz_*. Returns 1 on success.
   Only used in VT/ConPTY mode where GetConsoleScreenBufferInfo is unavailable. */
static int vt_query_size(void)
{
    HANDLE hin = GetStdHandle(STD_INPUT_HANDLE);
    HANDLE hout = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD w = 0;
    int i, tot = 0;
    char buf[64];
    unsigned char rb[64];
    int rows = 0, cols = 0, stage = 0, val = 0;

    /* Request window size in characters: ESC[18t -> response ESC[8;h;w t */
    const char* req = "\x1b[18t";
    WriteFile(hout, req, (DWORD)strlen(req), &w, NULL);

    /* Read the synchronous report echoed back on stdin (pipe).
       Bound each read with a short timeout so we never hang at boot if the
       terminal does not answer the size query. */
    for (i = 0; i < 24; i++) {
        if (WaitForSingleObject(hin, 150) != WAIT_OBJECT_0) break;   /* no data -> give up */
        if (!ReadFile(hin, rb, 1, &w, NULL) || w == 0) break;
        buf[tot < 63 ? tot : 63] = (char)rb[0];
        if (tot < 63) tot++;
        /* Parse ESC [ 8 ; rows ; cols t  (response to ESC[18t) */
        if (stage == 0 && rb[0] == 0x1b) stage = 1;
        else if (stage == 1 && rb[0] == '[') stage = 2;
        else if (stage == 2 && rb[0] == '8') { stage = 3; }       /* expect ';' then rows */
        else if (stage == 3 && rb[0] == ';') { stage = 4; val = 0; } /* start of rows */
        else if (stage == 4) {
            if (rb[0] >= '0' && rb[0] <= '9') val = val * 10 + (rb[0] - '0');
            else if (rb[0] == ';') { rows = val; stage = 5; val = 0; } /* start of cols */
            else { stage = 0; }
        }
        else if (stage == 5) {
            if (rb[0] >= '0' && rb[0] <= '9') val = val * 10 + (rb[0] - '0');
            else if (rb[0] == 't') { cols = val; stage = 6; break; }
            else { stage = 0; }
        }
        if (stage == 6) break;
    }
    buf[tot] = 0;
    if (rows > 0 && cols > 0) {
        g_sz_rows = rows; g_sz_cols = cols; g_sz_ok = 1;
        aura_dbg("vt_query_size: rows=%d cols=%d", rows, cols);
        return 1;
    }
    return 0;
}

/* Re-read the real terminal size (call after a resize). Returns 1 if changed. */
int aura_con_refresh_size(void)
{
    int oldc = g_sz_cols, oldr = g_sz_rows, ok = g_sz_ok;
    g_sz_ok = 0;
    if (is_vt_mode()) {
        if (!vt_query_size()) { g_sz_ok = ok; g_sz_cols = oldc; g_sz_rows = oldr; return 0; }
        return (g_sz_cols != oldc || g_sz_rows != oldr) ? 1 : 0;
    }
    int c = 0, r = 0;
    aura_con_get_size(&c, &r);
    return (c != oldc || r != oldr) ? 1 : 0;
}

/* Best-effort ConPTY resize detection: peek the console input buffer (CONIN$)
   for a WINDOW_BUFFER_SIZE_EVENT. In a ConPTY the stdin is a pipe (so the byte
   reader never sees this event) but the pseudoconsole's input buffer still
   receives it. Returns 1 if a resize was observed (event consumed). Safe: if
   CONIN$ is unavailable we simply report no resize. */
static int vt_check_resize(void)
{
    static HANDLE hConIn2 = NULL;
    static int tried = 0;
    INPUT_RECORD rec;
    DWORD n = 0;
    if (!tried) {
        tried = 1;
        hConIn2 = CreateFileA("CONIN$", GENERIC_READ,
                              FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
                              OPEN_EXISTING, 0, NULL);
        if (hConIn2 == INVALID_HANDLE_VALUE) hConIn2 = NULL;
    }
    if (!hConIn2) return 0;
    if (!PeekConsoleInputW(hConIn2, &rec, 1, &n) || n == 0) return 0;
    if (rec.EventType == WINDOW_BUFFER_SIZE_EVENT) {
        ReadConsoleInputW(hConIn2, &rec, 1, &n);   /* consume */
        return 1;
    }
    return 0;
}

void aura_con_hide_cursor(void) {
    ensure_handles();
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD w; const char* s = "\x1b[?25l";
    WriteFile(out, s, (DWORD)strlen(s), &w, NULL);
}
void aura_con_show_cursor(void) {
    ensure_handles();
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD w; const char* s = "\x1b[?25h";
    WriteFile(out, s, (DWORD)strlen(s), &w, NULL);
}

/* Move the host cursor (0-based col/row). */
void aura_con_set_cursor(int col, int row)
{
    ensure_handles();
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    char seq[32]; DWORD w;
    int plen = snprintf(seq, sizeof(seq), "\x1b[%d;%dH", row + 1, col + 1);
    WriteFile(out, seq, (DWORD)plen, &w, NULL);
}

/* Write raw bytes straight to the console (for VT init sequences) */
void aura_con_write_raw(const char* bytes, int n)
{
    DWORD w;
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    if (out == INVALID_HANDLE_VALUE || out == NULL) return;
    /* In a real console, enable VT so our ESC sequences render; in a ConPTY
       (pipe) this is a no-op and WriteFile delivers bytes to the host. */
    if (!is_vt_mode()) {
        DWORD m = 0;
        if (GetConsoleMode(out, &m)) {
            SetConsoleMode(out, m | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
        }
    }
    WriteFile(out, bytes, (DWORD)n, &w, NULL);
}


int aura_msgbox(const char* text, const char* title)
{
    /* Force a console so the message is visible even if std handles were detached */
    if (GetConsoleWindow() == NULL) {
        AttachConsole(ATTACH_PARENT_PROCESS);
        if (GetConsoleWindow() == NULL) AllocConsole();
    }
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    if (out != INVALID_HANDLE_VALUE && out != NULL) {
        DWORD w;
        SetConsoleTextAttribute(out, FOREGROUND_RED | FOREGROUND_INTENSITY);
        WriteConsoleA(out, "AURA ERROR:\\r\\n", 14, &w, NULL);
        WriteConsoleA(out, text, (DWORD)strlen(text), &w, NULL);
        WriteConsoleA(out, "\\r\\n", 2, &w, NULL);
    }
    /* Also pop a GUI dialog so it is impossible to miss */
    MessageBoxA(NULL, text, title, MB_OK | MB_ICONERROR);
    return 0;
}

#else
/* Unix: termios raw input; keys via read() escape decoding done Fortran-side */
#include <termios.h>
#include <unistd.h>
#include <sys/select.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
    int ev_type;      /* 0 none, 1 char, 2 special, 3 resize */
    unsigned int ch;  /* UCS codepoint or raw byte */
    int ev_key;       /* same numbering as Windows */
    int ctrl, shift, alt;
} AuraKeyEv;

static struct termios old_tio;
static int saved = 0;

void aura_con_raw_enter(void)
{
    struct termios tio;
    if (tcgetattr(STDIN_FILENO, &old_tio) == 0) saved = 1;
    tio = old_tio;
    tio.c_lflag &= ~(ICANON | ECHO | ISIG);
    tio.c_iflag &= ~(IXON | ICRNL);
    tio.c_cc[VMIN] = 0; tio.c_cc[VTIME] = 0;
    tcsetattr(STDIN_FILENO, TCSANOW, &tio);
}
void aura_con_raw_exit(void) { if (saved) tcsetattr(STDIN_FILENO, TCSANOW, &old_tio); }

/* decode simple CSI arrows/pgup/pgdn */
int aura_con_poll_key(int wait_ms, AuraKeyEv* out)
{
    fd_set fds;
    struct timeval tv;
    char buf[8];
    int n;

    memset(out, 0, sizeof(*out));
    FD_ZERO(&fds); FD_SET(STDIN_FILENO, &fds);
    tv.tv_sec = wait_ms / 1000; tv.tv_usec = (wait_ms % 1000) * 1000;
    if (select(STDIN_FILENO + 1, &fds, NULL, NULL, &tv) <= 0) return 0;
    n = (int)read(STDIN_FILENO, buf, sizeof(buf));
    if (n <= 0) return 0;
    if (buf[0] == '\x1b' && n >= 3 && buf[1] == '[') {
        out->ev_type = 2;
        switch (buf[2]) {
        case 'A': out->ev_key = 1; break;
        case 'B': out->ev_key = 2; break;
        case 'C': out->ev_key = 4; break;
        case 'D': out->ev_key = 3; break;
        case 'H': out->ev_key = 5; break;
        case 'F': out->ev_key = 6; break;
        case '5': out->ev_key = 7; break;   /* PgUp ~ ESC[5~ */
        case '6': out->ev_key = 8; break;
        default: out->ev_type = 0; break;
        }
        return out->ev_type ? 1 : 0;
    }
    out->ev_type = 1;
    out->ch = (unsigned char)buf[0];
    if (buf[0] >= 1 && buf[0] <= 26 && buf[0] != '\r' && buf[0] != '\t' && buf[0] != '\n') {
        out->ctrl = 1;
        out->ch = (unsigned char)(buf[0] + 'a' - 1);  /* normalize to letter */
    }
    return 1;
}

void aura_con_write_at(int col, int row, const short* text_utf16, int nchars) { (void)col;(void)row;(void)text_utf16;(void)nchars; }
void aura_con_get_size(int* cols, int* rows)
{
    struct winsize ws;
    if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == 0) { *cols = ws.ws_col; *rows = ws.ws_row; }
    else { *cols = 80; *rows = 24; }
}
void aura_con_hide_cursor(void) {}
void aura_con_show_cursor(void) {}
#endif

/* --- flat scalar wrapper for Fortran bind(C) ---------------------------- */
int aura_con_poll_key_wrapper(int wait_ms, int* ev_type, int* ch, int* spk,
                              int* ctrl, int* shift, int* alt)
{
    AuraKeyEv ev;
    int got = aura_con_poll_key(wait_ms, &ev);
    *ev_type = ev.ev_type; *ch = (int)ev.ch; *spk = ev.ev_key;
    *ctrl = ev.ctrl; *shift = ev.shift; *alt = ev.alt;
    return got;
}

