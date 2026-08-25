/* aura_pty_win.c — ConPTY backend (Windows 10 1809+)
 * All ConPTY APIs resolved dynamically via kernel32 GetProcAddress.
 */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>

typedef void* HPCON;

#ifndef PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE
#define PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE 0x00020016
#endif

typedef HRESULT (WINAPI *PFN_CreatePseudoConsole)(COORD, HANDLE, HANDLE, DWORD, HPCON*);
typedef HRESULT (WINAPI *PFN_ResizePseudoConsole)(HPCON, COORD);
typedef void   (WINAPI *PFN_ClosePseudoConsole)(HPCON);

typedef struct {
    HPCON pc;
    HANDLE hPipeIn;
    HANDLE hPipeOut;
    PROCESS_INFORMATION pi;
    BOOL alive;
} AuraPtyWin;

static PFN_CreatePseudoConsole fnCreate = NULL;
static PFN_ResizePseudoConsole fnResize = NULL;
static PFN_ClosePseudoConsole  fnClose  = NULL;

static int load_conpty(void)
{
    HMODULE k32 = GetModuleHandleA("kernel32.dll");
    if (!fnCreate) fnCreate = (PFN_CreatePseudoConsole)GetProcAddress(k32, "CreatePseudoConsole");
    if (!fnResize) fnResize = (PFN_ResizePseudoConsole)GetProcAddress(k32, "ResizePseudoConsole");
    if (!fnClose)  fnClose  = (PFN_ClosePseudoConsole)GetProcAddress(k32, "ClosePseudoConsole");
    return fnCreate && fnResize && fnClose;
}

long long aura_pty_spawn(const char* cmdline, int cols, int rows, long long* handle_out)
{
    AuraPtyWin* p;
    COORD sz;
    HANDLE hInR = NULL, hInW = NULL, hOutR = NULL, hOutW = NULL;
    STARTUPINFOEXW si;
    SIZE_T attrSize = 0;
    wchar_t wcmdline[2048];
    HRESULT hr;
    BYTE attrBuf[64];

    if (!load_conpty()) goto fail;
    p = (AuraPtyWin*)calloc(1, sizeof(AuraPtyWin));
    if (!p) goto fail2;
    sz.X = (SHORT)cols; sz.Y = (SHORT)rows;

    if (!CreatePipe(&hOutR, &hOutW, NULL, 0)) goto fail2;
    if (!CreatePipe(&hInR, &hInW, NULL, 0)) goto fail2;

    hr = fnCreate(sz, hInR, hOutW, 0, &p->pc);
    if (FAILED(hr)) goto fail2;

    ZeroMemory(&si, sizeof(si));
    si.StartupInfo.cb = sizeof(si);
    si.StartupInfo.dwFlags = STARTF_USESTDHANDLES;   /* REQUIRED: wire child stdio to ConPTY */
    InitializeProcThreadAttributeList(NULL, 1, 0, &attrSize);
    si.lpAttributeList = (LPPROC_THREAD_ATTRIBUTE_LIST)attrBuf;
    if (attrSize > sizeof(attrBuf)) goto fail2;
    if (!InitializeProcThreadAttributeList(si.lpAttributeList, 1, 0, &attrSize)) goto fail2;
    if (!UpdateProcThreadAttribute(si.lpAttributeList, 0,
            PROC_THREAD_ATTRIBUTE_PSEUDOCONSOLE,
            p->pc, sizeof(p->pc), NULL, NULL)) goto fail2;

    MultiByteToWideChar(CP_UTF8, 0, cmdline, -1, wcmdline, 2048);

    if (!CreateProcessW(NULL, wcmdline, NULL, NULL, FALSE,
            EXTENDED_STARTUPINFO_PRESENT, NULL, NULL,
            &si.StartupInfo, &p->pi)) goto fail2;

    DeleteProcThreadAttributeList(si.lpAttributeList);

    /* keep child-side ends open until after CreateProcess, then close ours */
    CloseHandle(hInR); CloseHandle(hOutW);

    p->hPipeIn = hInW;
    p->hPipeOut = hOutR;
    p->alive = TRUE;
    *handle_out = (long long)p;
    return 0;

fail2:
    if (p) free(p);
fail:
    if (hInR) CloseHandle(hInR);
    if (hInW) CloseHandle(hInW);
    if (hOutR) CloseHandle(hOutR);
    if (hOutW) CloseHandle(hOutW);
    *handle_out = 0;
    return -1;
}

int aura_pty_read(long long handle, char* buf, int buflen)
{
    AuraPtyWin* p = (AuraPtyWin*)handle;
    DWORD n = 0;
    if (!p || !p->alive) return -1;
    if (!PeekNamedPipe(p->hPipeOut, NULL, 0, NULL, &n, NULL)) {
        p->alive = FALSE;
        return -1;
    }
    if (n == 0) return 0;
    if (n > (DWORD)buflen) n = (DWORD)buflen;
    if (!ReadFile(p->hPipeOut, buf, n, &n, NULL)) {
        p->alive = FALSE;
        return -1;
    }
    return (int)n;
}

int aura_pty_write(long long handle, const char* buf, int nbytes)
{
    AuraPtyWin* p = (AuraPtyWin*)handle;
    DWORD written = 0;
    if (!p || !p->alive) return -1;
    if (!WriteFile(p->hPipeIn, buf, (DWORD)nbytes, &written, NULL)) {
        p->alive = FALSE;
        return -1;
    }
    return (int)written;
}

void aura_pty_resize(long long handle, int cols, int rows)
{
    AuraPtyWin* p = (AuraPtyWin*)handle;
    COORD sz;
    if (!p || !fnResize) return;
    sz.X = (SHORT)cols; sz.Y = (SHORT)rows;
    fnResize(p->pc, sz);
}

void aura_pty_close(long long handle)
{
    AuraPtyWin* p = (AuraPtyWin*)handle;
    if (!p) return;
    if (fnClose) fnClose(p->pc);
    TerminateProcess(p->pi.hProcess, 0);
    CloseHandle(p->pi.hProcess);
    CloseHandle(p->pi.hThread);
    CloseHandle(p->hPipeIn);
    CloseHandle(p->hPipeOut);
    free(p);
}

#else /* !_WIN32 */

#include <pty.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <signal.h>
#include <fcntl.h>

typedef struct {
    int master;
    pid_t pid;
    int alive;
} AuraPtyUnix;

long long aura_pty_spawn(const char* cmdline, int cols, int rows, long long* handle_out)
{
    AuraPtyUnix* p = (AuraPtyUnix*)calloc(1, sizeof(AuraPtyUnix));
    struct winsize ws;
    if (!p) { *handle_out = 0; return -1; }
    ws.ws_col = (unsigned short)cols;
    ws.ws_row = (unsigned short)rows;
    p->pid = forkpty(&p->master, NULL, NULL, &ws);
    if (p->pid < 0) { free(p); *handle_out = 0; return -1; }
    if (p->pid == 0) {
        setenv("TERM", "xterm-256color", 1);
        execl("/bin/sh", "sh", "-c", cmdline, (char*)NULL);
        _exit(127);
    }
    fcntl(p->master, F_SETFL, O_NONBLOCK);
    p->alive = 1;
    *handle_out = (long long)p;
    return 0;
}

int aura_pty_read(long long handle, char* buf, int buflen)
{
    AuraPtyUnix* p = (AuraPtyUnix*)handle;
    ssize_t n;
    int status;
    if (!p || !p->alive) return -1;
    n = read(p->master, buf, buflen);
    if (n > 0) return (int)n;
    if (n == 0) { p->alive = 0; return -1; }
    if (waitpid(p->pid, &status, WNOHANG) != 0) { p->alive = 0; return -1; }
    return 0;
}

int aura_pty_write(long long handle, const char* buf, int nbytes)
{
    AuraPtyUnix* p = (AuraPtyUnix*)handle;
    if (!p || !p->alive) return -1;
    return (int)write(p->master, buf, nbytes);
}

void aura_pty_resize(long long handle, int cols, int rows)
{
    AuraPtyUnix* p = (AuraPtyUnix*)handle;
    struct winsize ws;
    if (!p) return;
    ws.ws_col = (unsigned short)cols;
    ws.ws_row = (unsigned short)rows;
    ioctl(p->master, TIOCSWINSZ, &ws);
}

void aura_pty_close(long long handle)
{
    AuraPtyUnix* p = (AuraPtyUnix*)handle;
    if (!p) return;
    if (p->alive) kill(p->pid, SIGTERM);
    close(p->master);
    free(p);
}
#endif
