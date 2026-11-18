param([switch]$NoBrowser)
$ErrorActionPreference = 'Stop'
$guiRoot = $PSScriptRoot
$guiUrl = 'http://127.0.0.1:8765'
try {
    try {
        $existingGui = Invoke-RestMethod -Uri "$guiUrl/api/defaults" -TimeoutSec 2
        if ($existingGui.rules -contains 'Reg-Move') {
            if (-not $NoBrowser) { Start-Process $guiUrl }
            exit 0
        }
    } catch { }
    $guiPython = $env:BRP_GUI_PYTHON
    $pythonPathFile = Join-Path $guiRoot 'python-path.txt'
    if (-not $guiPython -and (Test-Path -LiteralPath $pythonPathFile)) {
        $guiPython = (Get-Content -LiteralPath $pythonPathFile -Raw).Trim()
    }
    if (-not $guiPython) {
        foreach ($candidate in @('python.exe', 'python3.exe')) {
            $foundPython = Get-Command $candidate -ErrorAction SilentlyContinue
            if ($foundPython -and $foundPython.Source -notlike '*\Microsoft\WindowsApps\*') {
                $guiPython = $foundPython.Source
                break
            }
        }
    }
    if (-not $guiPython) { throw 'Python 3.10 or newer was not found. Set BRP_GUI_PYTHON to python.exe or install Python.' }
    $guiArguments = @(('"' + (Join-Path $guiRoot 'app.py') + '"'), '--no-browser')
    $guiProcess = Start-Process -FilePath $guiPython -ArgumentList $guiArguments -WorkingDirectory $guiRoot -WindowStyle Hidden -PassThru -RedirectStandardOutput (Join-Path $guiRoot 'server.log') -RedirectStandardError (Join-Path $guiRoot 'server-error.log')
    $guiProcess.Id | Set-Content -LiteralPath (Join-Path $guiRoot 'server.pid')
    @{id=$guiProcess.Id; executable=$guiProcess.Path; start_ticks=$guiProcess.StartTime.ToUniversalTime().Ticks; script=(Join-Path $guiRoot 'app.py')} | ConvertTo-Json | Set-Content -LiteralPath (Join-Path $guiRoot 'server-process.json')
    $ready = $false
    for ($attempt = 0; $attempt -lt 35; $attempt++) {
        Start-Sleep -Milliseconds 250
        try {
            $startedGui = Invoke-RestMethod -Uri "$guiUrl/api/defaults" -TimeoutSec 1
            if ($startedGui.rules -contains 'Reg-Move') { $ready = $true; break }
        } catch { }
        if ($guiProcess.HasExited) { break }
    }
    if (-not $ready) {
        $guiErrorLog = Join-Path $guiRoot 'server-error.log'
        $guiFailure = if (Test-Path -LiteralPath $guiErrorLog) { Get-Content -LiteralPath $guiErrorLog -Raw } else { 'Server did not become ready.' }
        throw "The GUI could not start. $guiFailure"
    }
    if (-not $NoBrowser) { Start-Process $guiUrl }
} catch {
    Add-Type -AssemblyName System.Windows.Forms
    [System.Windows.Forms.MessageBox]::Show($_.Exception.Message, 'Spatial BRP GUI') | Out-Null
    exit 1
}
