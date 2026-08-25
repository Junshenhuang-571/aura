#include <windows.h>
#include <stdio.h>
int main(void){
    STARTUPINFO si; PROCESS_INFORMATION pi;
    char cmd[] = "build\aura.exe";
    char dir[MAX_PATH];
    GetCurrentDirectoryA(MAX_PATH, dir);
    ZeroMemory(&si,sizeof(si)); si.cb=sizeof(si);
    if(!CreateProcessA(NULL,cmd,NULL,NULL,FALSE,0,NULL,dir,&si,&pi)){
        printf("CreateProcess failed %lu\n",GetLastError()); return 1;
    }
    WaitForSingleObject(pi.hProcess, 4000);
    DWORD code=0; GetExitCodeProcess(pi.hProcess,&code);
    printf("aura exited code=%lu\n",code);
    CloseHandle(pi.hProcess); CloseHandle(pi.hThread);
    return 0;
}
