#include <windows.h>
#include <stdio.h>
int main(void){
    /* try attaching to the parent console explicitly */
    if (!AttachConsole(ATTACH_PARENT_PROCESS)) {
        printf("AttachConsole(parent) failed %lu\n", GetLastError());
        return 1;
    }
    HANDLE in = GetStdHandle(STD_INPUT_HANDLE);
    DWORD mode;
    CONSOLE_SCREEN_BUFFER_INFO info;
    if (!GetConsoleMode(in, &mode)) printf("in bad err=%lu\n", GetLastError());
    else printf("attached OK in-mode=%lu\n", (unsigned long)mode);
    if (GetConsoleScreenBufferInfo(GetStdHandle(STD_OUTPUT_HANDLE), &info))
        printf("size=%dx%d\n", info.srWindow.Right-info.srWindow.Left+1, info.srWindow.Bottom-info.srWindow.Top+1);
    return 0;
}
