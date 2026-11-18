$ErrorActionPreference = 'Stop'
$guiMetadataPath = Join-Path $PSScriptRoot 'server-process.json'
if (Test-Path -LiteralPath $guiMetadataPath) {
    $guiMetadata = Get-Content -LiteralPath $guiMetadataPath -Raw | ConvertFrom-Json
    $guiProcess = Get-Process -Id ([int]$guiMetadata.id) -ErrorAction SilentlyContinue
    $expectedGuiScript = Join-Path $PSScriptRoot 'app.py'
    if ($guiProcess -and $guiMetadata.script -eq $expectedGuiScript -and $guiProcess.Path -eq $guiMetadata.executable -and $guiProcess.StartTime.ToUniversalTime().Ticks -eq $guiMetadata.start_ticks) {
        Stop-Process -Id $guiProcess.Id
    }
}
