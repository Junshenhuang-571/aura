Set-Location $env:USERPROFILE\projects\aura
& .\build\conprobe.exe
Write-Output ("exit=" + $LASTEXITCODE)
