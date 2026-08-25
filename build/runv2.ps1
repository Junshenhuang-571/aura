Set-Location $env:USERPROFILE\projects\aura
$p = Start-Process -FilePath ".\build\aura_v3.exe" -ArgumentList "--config" -NoNewWindow -Wait -PassThru -RedirectStandardOutput "build\ps_out.txt" -RedirectStandardError "build\ps_err.txt"
Write-Output ("exitcode=" + $p.ExitCode)
Write-Output ("stdout: " + (Get-Content build\ps_out.txt -Raw))
