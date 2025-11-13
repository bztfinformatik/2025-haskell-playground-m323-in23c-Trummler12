param([Parameter(ValueFromRemainingArguments=$true)][string[]]$Args)

# Auto-discovery Wrapper für Cabal REPL
# Lädt automatisch alle Library-Pakete oder ein spezifisches Paket
# Unterstützt: --list (Paketliste anzeigen)

# Suche lokale Cabal-Dateien (ohne dist-newstyle)
$cabals = Get-ChildItem -Path . -Recurse -Depth 2 -Filter *.cabal `
  | Where-Object { $_.FullName -notmatch '\\dist-newstyle\\' } `
  | Sort-Object FullName

$allPkgs = @()
$libPkgs = @()

foreach ($c in $cabals) {
  $name = Select-String -Path $c.FullName -Pattern '(?i)^\s*name:\s*(\S+)' `
    | ForEach-Object { $_.Matches[0].Groups[1].Value } | Select-Object -First 1
  if (-not $name) { continue }

  $allPkgs += $name
  $hasLib = Select-String -Path $c.FullName -Pattern '(?i)^\s*library(\s|$)'
  if ($hasLib) { $libPkgs += $name }
}

function Join-Csv([string[]]$xs) { ($xs -join ',') }

# Service-Befehl: --list
if ($Args.Count -eq 1 -and $Args[0] -eq '--list') {
  Write-Host "Library packages: $($libPkgs -join ', ')" -ForegroundColor Green
  Write-Host "All packages: $($allPkgs -join ', ')" -ForegroundColor Cyan
  exit 0
}

# Helper: Get absolute path to .ghci script
function Get-GhciScript([string]$name) {
  $script = ".ghci-$name"
  if (Test-Path $script) {
    return (Resolve-Path $script).Path
  }
  return (Resolve-Path ".ghci").Path
}

if ($Args.Count -eq 0) {
  # Load all libraries via demos executable
  if ($allPkgs -contains 'demos') {
    Write-Host "Loading all libraries via demos executable" -ForegroundColor Cyan
    $ghciScript = Get-GhciScript "all"
    cabal repl demos:exe:demo --ghc-options="-ghci-script=$ghciScript"
  } elseif ($libPkgs.Count -eq 0) { 
    Write-Error "Keine Library-Pakete gefunden."
    exit 1 
  } else {
    Write-Error "Demo-Paket nicht gefunden. Bitte 'cabal build demos' ausführen."
    exit 1
  }
} elseif ($Args.Count -eq 1) {
  # Single package
  $pkg = $Args[0]
  if ($allPkgs -contains $pkg) {
    Write-Host "Loading library: $pkg" -ForegroundColor Cyan
    $ghciScript = Get-GhciScript $pkg
    cabal repl "lib:$pkg" --ghc-options="-ghci-script=$ghciScript"
  } else {
    # Pass through to cabal repl (e.g., exe:demo, test:tests)
    $ghciScript = (Resolve-Path ".ghci").Path
    cabal repl $pkg --ghc-options="-ghci-script=$ghciScript"
  }
} else {
  # Multi-package: check if all args are packages
  $allArePkgs = $true
  foreach ($arg in $Args) {
    if (-not ($allPkgs -contains $arg)) {
      $allArePkgs = $false
      break
    }
  }
  
  if ($allArePkgs) {
    Write-Host "Loading libraries: $($Args -join ', ') (requires multi-repl)" -ForegroundColor Cyan
    $targets = $Args | ForEach-Object { "lib:$_" }
    $ghciScript = Get-GhciScript "all"
    cabal repl --enable-multi-repl @targets --ghc-options="-ghci-script=$ghciScript"
  } else {
    # Pass through
    $ghciScript = (Resolve-Path ".ghci").Path
    cabal repl @Args --ghc-options="-ghci-script=$ghciScript"
  }
}
