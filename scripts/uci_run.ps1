param(
    [Parameter(Position = 0, ValueFromRemainingArguments = $true)][string[]]$Cmd,
    [double]$Timeout = 60,
    [string]$Exe,
    [string]$EngineArgs
)

$ErrorActionPreference = 'Stop'

if (-not $Exe) {
    $scripts = if ($PSScriptRoot) { $PSScriptRoot } else { Split-Path -Parent $MyInvocation.MyCommand.Path }
    $Exe = [IO.Path]::GetFullPath((Join-Path $scripts '..\target\release\simd-chess.exe'))
}
if (-not $Cmd) { $Cmd = @('uci', 'position startpos', 'go movetime 1000') }
if (-not (Test-Path -LiteralPath $Exe)) { Write-Error "missing $Exe (run: cargo build -r)"; exit 2 }

$prevIn = [Console]::InputEncoding
try { [Console]::InputEncoding = New-Object System.Text.ASCIIEncoding } catch {}

$psi = New-Object System.Diagnostics.ProcessStartInfo
$psi.FileName = $Exe
if ($EngineArgs) { $psi.Arguments = $EngineArgs }
$psi.RedirectStandardInput = $true
$psi.RedirectStandardOutput = $true
$psi.UseShellExecute = $false
$p = [System.Diagnostics.Process]::Start($psi)

$expected = ($Cmd | Where-Object { $_ -match '^\s*go\b' -and $_ -notmatch 'infinite' }).Count

try {
    foreach ($c in $Cmd) { $p.StandardInput.WriteLine($c) }
    if ($expected -eq 0) { $p.StandardInput.WriteLine('quit') }
    $p.StandardInput.Flush()
} catch {
    Write-Error "[uci_run] engine closed stdin early: $($_.Exception.Message)"
}

$deadline = [DateTime]::UtcNow.AddSeconds($Timeout)
$seen = 0
$rc = 0
$quitSent = $false

while ($true) {
    $remain = ($deadline - [DateTime]::UtcNow).TotalMilliseconds
    if ($remain -le 0) { Write-Error '[uci_run] TIMEOUT -> killing'; $rc = 124; break }

    $task = $p.StandardOutput.ReadLineAsync()
    if (-not $task.Wait([int][Math]::Min($remain, [double][int]::MaxValue))) {
        Write-Error '[uci_run] TIMEOUT -> killing'; $rc = 124; break
    }

    $line = $task.Result
    if ($null -eq $line) { break }
    Write-Output $line

    if ($line -match 'bestmove') {
        $seen++
        if ($seen -ge $expected -and -not $quitSent) {
            try { $p.StandardInput.WriteLine('quit'); $p.StandardInput.Flush() } catch {}
            $quitSent = $true
        }
    }
}

if (-not $p.HasExited) { $p.Kill() }
$p.WaitForExit(3000) | Out-Null
$p.Dispose()
try { [Console]::InputEncoding = $prevIn } catch {}
exit $rc
