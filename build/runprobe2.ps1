$code = @'
#include <windows.h>
#include <stdio.h>
int main(void){
    DWORD mode;
    HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (!GetConsoleMode(in, &mode)) printf("in handle bad err=%lu\n", GetLastError());
    else printf("console OK in-mode=%lu\n", (unsigned long)mode);
    if (!GetConsoleScreenBufferInfo(out, &info)) printf("out bad err=%lu\n", GetLastError());
    else printf("size=%dx%d\n", info.srWindow.Right-info.srWindow.Left+1, info.srWindow.Bottom-info.srWindow.Top+1);
    return 0;
}
'@
Set-Content -Path "$env:USERPROFILE\projects\aura\build\conprobe2.c" -Value $code
& gcc -o "$env:USERPROFILE\projects\aura\build\conprobe2.exe" "$env:USERPROFILE\projects\aura\build\conprobe2.c"
# Attach to the user's ACTIVE console session via a new visible window:
Start-Process -FilePath "conhost.exe" -ArgumentList "--headless" -WindowStyle Hidden
& "$env:USERPROFILE\projects\aura\build\conprobe2.exe"
