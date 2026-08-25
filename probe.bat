@echo off
REM Aura environment probe — run this in the SAME window where you typed ".\build\aura.exe"
echo === Aura host probe ===
echo CONEMU=%CONEMU%
echo WT_SESSION=%WT_SESSION%
echo TERM_PROGRAM=%TERM_PROGRAM%
echo TERM=%TERM%
echo PSModulePath set? (windows terminal check via title)
echo.
echo --- where is aura.exe ---
dir build\aura.exe 2>nul || dir aura.exe 2>nul
echo.
echo --- run aura for 4s, capture exit code ---
timeout /t 4 >nul <nul | rem
where timeout >nul 2>nul && echo timeout-ok
echo.
echo === Now I will launch Aura via Start-Process (its own console) ===
powershell -NoProfile -Command "Start-Process -FilePath '.\build\aura.exe' -WindowStyle Normal; Start-Sleep 4; $p=Get-Process aura -ErrorAction SilentlyContinue; if($p){Write-Output 'AURA_ALIVE pid='+$p.Id}else{Write-Output 'AURA_DEAD'}"
echo.
echo === aura_boot.log (if any) ===
if exist aura_boot.log (type aura_boot.log) else (echo NO aura_boot.log created)
