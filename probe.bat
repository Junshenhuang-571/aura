@echo off
REM Run this EXACTLY as written, in the window where you typed ".\build\aura.exe"
cd /d C:\Users\junsh\projects\aura

echo === Aura host probe ===
echo CONEMU=%CONEMU%
echo WT_SESSION=%WT_SESSION%
echo TERM_PROGRAM=%TERM_PROGRAM%
echo TERM=%TERM%
echo PSModulePath(contains WindowsTerminal?)=%PSModulePath%
echo.

echo === 1. Does the static binary even start here? (4s window) ===
start "Aura" cmd /c "build\aura.exe & pause"
echo    (a new window titled 'Aura' should appear — did it? what does it show?)
echo.

echo === 2. boot log after a manual run attempt ===
if exist aura_boot.log (echo --- aura_boot.log --- & type aura_boot.log) else (echo NO aura_boot.log)
echo.
echo === 3. any missing-DLL style error? ===
build\aura.exe --config
echo    exit code of --config = %ERRORLEVEL%
echo.
echo DONE. Copy everything above back to me.
pause
