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

static void ensure_handles(void) {
    if (!hConIn) {
        /* If our std handles aren't a console (detached/redirected),
           attach to the parent's console or allocate a new one. */
        if (GetConsoleWindow() == NULL) {
            AttachConsole(ATTACH_PARENT_PROCESS);
        }
        hConIn = GetStdHandle(STD_INPUT_HANDLE);
        hConOut = GetStdHandle(STD_OUTPUT_HANDLE);
        /* Reopen std handles to the console if they were redirected/invalid */
        if (GetConsoleMode(hConIn, &oldInMode) == 0) {
            freopen("CONIN$", "rb", stdin);
            hConIn = (HANDLE)_get_osfhandle(_fileno(stdin));
            GetConsoleMode(hConIn, &oldInMode);
        }
        if (GetConsoleMode(hConOut, &oldOutMode) == 0) {
            freopen("CONOUT$", "wb", stdout);
            hConOut = (HANDLE)_get_osfhandle(_fileno(stdout));
            GetConsoleMode(hConOut, &oldOutMode);
        }
    }
}

void aura_con_raw_enter(void)
{
    DWORD mode;
    ensure_handles();
    mode = oldInMode;
    mode &= ~(ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT | ENABLE_PROCESSED_INPUT);
    mode |= ENABLE_WINDOW_INPUT;              /* want resize events */
    SetConsoleMode(hConIn, mode);
    /* enable VT output on the legacy path too (harmless under WT) */
    mode = oldOutMode | ENABLE_VIRTUAL_TERMINAL_PROCESSING;
    SetConsoleMode(hConOut, mode);
}

void aura_con_raw_exit(void)
{
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
    DWORD written = 0;
    COORD pos;
    pos.X = (SHORT)(col - 1);      /* Fortran side sends 0-based already? keep 0-based contract */
    pos.Y = (SHORT)(row - 1);      /* NOTE: see header comment — row here is 0-based from caller */
    SetConsoleCursorPosition(hConOut, pos);
    WriteConsoleOutputCharacterW(hConOut, text, (DWORD)nchars, pos, &written);
}

void aura_con_get_size(int* cols, int* rows)
{
    CONSOLE_SCREEN_BUFFER_INFO info;
    ensure_handles();
    if (GetConsoleScreenBufferInfo(hConOut, &info)) {
        *cols = (int)(info.srWindow.Right - info.srWindow.Left + 1);
        *rows = (int)(info.srWindow.Bottom - info.srWindow.Top + 1);
    } else {
        *cols = 80; *rows = 25;
    }
}

void aura_con_hide_cursor(void) { CONSOLE_CURSOR_INFO ci; ci.dwSize=25; ci.bVisible=FALSE; SetConsoleCursorInfo(hConOut,&ci); }
void aura_con_show_cursor(void) { CONSOLE_CURSOR_INFO ci; ci.dwSize=25; ci.bVisible=TRUE; SetConsoleCursorInfo(hConOut,&ci); }

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
