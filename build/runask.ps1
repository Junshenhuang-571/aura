Set-Location $env:USERPROFILE\projects\aura
& .\build\aura.exe --config
Write-Output ("exit=" + $LASTEXITCODE)
