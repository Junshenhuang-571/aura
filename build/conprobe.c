#include <windows.h>
#include <stdio.h>
int main(void){
    DWORD mode, outmode;
    HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
    HANDLE out = GetStdHandle(STD_OUTPUT_HANDLE);
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (!GetConsoleMode(in, &mode)) printf("no console input handle (piped?) err=%lu\n", GetLastError());
    else { printf("console OK, in-mode=%lu\n", (unsigned long)mode); }
    if (!GetConsoleScreenBufferInfo(out, &info)) printf("no screen buffer err=%lu\n", GetLastError());
    else printf("size=%dx%d\n", info.srWindow.Right-info.srWindow.Left+1, info.srWindow.Bottom-info.srWindow.Top+1);
    return 0;
}
