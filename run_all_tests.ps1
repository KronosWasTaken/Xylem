$ErrorActionPreference = 'Stop'

$exe = $null
if (Test-Path "target/release/xylem.exe") {
    $exe = "target/release/xylem.exe"
} elseif (Test-Path "target/debug/xylem.exe") {
    $exe = "target/debug/xylem.exe"
} elseif (Get-Command xylem -ErrorAction SilentlyContinue) {
    $exe = "xylem"
} else {
    Write-Host "Could not find xylem executable. Please build the project first." -ForegroundColor Red
    exit 1
}

$files = Get-ChildItem -Path tests -Filter *.xl
$failed = @()
foreach ($file in $files) {
    Write-Host "Running $($file.Name)..."
    & $exe $file.FullName
    if ($LASTEXITCODE -ne 0) {
        Write-Host "Test failed: $($file.Name) (exit code $LASTEXITCODE)" -ForegroundColor Red
        $failed += $file.Name
    }
    Write-Host ""
}
if ($failed.Count -eq 0) {
    Write-Host "All tests passed!" -ForegroundColor Green
} else {
    Write-Host "Failed tests:" -ForegroundColor Red
    $failed | ForEach-Object { Write-Host $_ -ForegroundColor Red }
    exit 1
} 