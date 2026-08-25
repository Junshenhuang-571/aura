Set-Location $env:USERPROFILE\projects\aura
# Launch aura in a NEW visible console window (its own conhost)
Start-Process -FilePath "$env:USERPROFILE\projects\aura\build\aura.exe" -WindowStyle Normal
Write-Output "launched"
