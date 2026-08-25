$code = @'
#include <windows.h>
#include <stdio.h>
int main(void){
    DWORD mode;
    CONSOLE_SCREEN_BUFFER_INFO info;
    /* Allocate our own console if we're detached from one */
    if (!GetConsoleWindow()) {
        if (!AllocConsole()) { printf("AllocConsole failed %lu\n", GetLastError()); return 1; }
        printf("allocated new console\n");
    }
    HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    if (!GetConsoleMode(in, &mode)) printf("in bad err=%lu\n", GetLastError());
    else printf("console OK in-mode=%lu\n", (unsigned long)mode);
    if (!GetConsoleScreenBufferInfo(out, &info)) printf("out bad err=%lu\n", GetLastError());
    else printf("size=%dx%d\n", info.srWindow.Right-info.srWindow.Left+1, info.srWindow.Bottom-info.srWindow.Top+1);
    return 0;
}
'@
Set-Content -Path "$env:USERPROFILE\projects\aura\build\conprobe3.c" -Value $code
& gcc -o "$env:USERPROFILE\projects\aura\build\conprobe3.exe" "$env:USERPROFILE\projects\aura\build\conprobe3.c"
& "$env:USERPROFILE\projects\aura\build\conprobe3.exe"
