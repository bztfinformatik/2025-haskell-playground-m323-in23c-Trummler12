param([Parameter(ValueFromRemainingArguments = $true)][string[]]$Args)

<#
.SYNOPSIS
  Auto-Discovery Wrapper für Cabal REPL im M323-Haskell Workspace

.DESCRIPTION
  Routing-Logik:
    hs                    -> demos:exe:demo + .ghci-all (alle Module: Geometry, NumLang, Utility)
    hs numlang            -> lib:numlang + .ghci (neutral, Cabal setzt Kontext)
    hs geometry           -> lib:geometry + .ghci (neutral)
    hs numlang geometry   -> Multi-REPL (experimental) + .ghci (neutral, manuelle Imports nötig)
    hs --list             -> Zeigt erkannte Pakete

  .ghci-Skripte:
    .ghci-all   - Aggregator: importiert alle Module + custom iprint
    .ghci       - Neutral: nur globale Flags (:set -Wall, Prompt)

.EXAMPLE
  hs                    # Lädt alles (demos + alle Libs)
  hs numlang            # Nur NumLang Library
  hs .\geometry\        # Path-Normalisierung: .\geometry\ -> geometry
#>

# 1) Cabal-Dateien finden (ohne dist-newstyle)
$cabals = Get-ChildItem -Path . -Recurse -Depth 2 -Filter *.cabal |
  Where-Object { $_.FullName -notmatch '\\dist-newstyle\\' } |
  Sort-Object FullName

$allPkgs = @()
$libPkgs = @()

foreach ($c in $cabals) {
  $nameMatch = Select-String -Path $c.FullName -Pattern '^\s*name:\s*(\S+)' -CaseSensitive:$false |
               Select-Object -First 1
  if (-not $nameMatch) { continue }
  $name = $nameMatch.Matches[0].Groups[1].Value
  if (-not $name) { continue }

  $allPkgs += $name

  $hasLib = Select-String -Path $c.FullName -Pattern '^\s*library(\s|$)' -CaseSensitive:$false |
            Select-Object -First 1
  if ($hasLib) {
    $libPkgs += $name
  }
}

# 2) Hilfsfunktionen

# Pfad-Argumente wie ".\numlang\" zu "numlang" normalisieren
function Normalize-Target([string]$t) {
  $t = $t.Trim()
  # führende .\ oder ./ oder ./
  $t = $t -replace '^[.\\/]+', ''
  # abschließende \ oder /
  $t = $t -replace '[\\/]+$', ''
  return $t
}

# passendes .ghci-Skript bestimmen
$rootGhci = (Resolve-Path "./.ghci").Path
function GhciScript-For([string]$pkg) {
  # Aggregator (kein Paket) -> .ghci-all (alle Imports)
  # Single-Lib (ein Paket)  -> .ghci (neutral, Cabal setzt Kontext)
  if ([string]::IsNullOrWhiteSpace($pkg)) {
    $p = "./.ghci-all"
  } else {
    $p = "./.ghci"
  }
  if (Test-Path $p) {
    return (Resolve-Path $p).Path
  } else {
    return $rootGhci
  }
}

# 3) Spezial-Flag --list
if ($Args.Count -eq 1 -and $Args[0] -eq '--list') {
  "Library packages: " + ($libPkgs -join ', ')
  "All packages: " + ($allPkgs -join ', ')
  exit 0
}

$hasDemos = Test-Path ".\demos\demos.cabal"

# 4) Argumente normalisieren (für Paket-Namen)
$normalized = @()
foreach ($a in $Args) {
  $normalized += (Normalize-Target $a)
}

# 5) Aufrufe

if ($normalized.Count -eq 0) {
  # hs  -> Aggregator: demos:exe:demo, sonst Scratch-REPL mit allen Libs
  $script = GhciScript-For ''
  if ($hasDemos) {
    cabal repl 'demos:exe:demo' --repl-options "-ghci-script=$script"
  } elseif ($libPkgs.Count -gt 0) {
    $depCsv = ($libPkgs -join ',')
    cabal repl --build-depends $depCsv --repl-options "-ghci-script=$script"
  } else {
    Write-Error "Keine Library-Pakete gefunden."
    exit 1
  }
}
elseif ($normalized.Count -eq 1 -and ($allPkgs -contains $normalized[0])) {
  # hs numlang  -> lib:numlang
  $pkg    = $normalized[0]
  $script = GhciScript-For $pkg
  cabal repl ("lib:" + $pkg) --repl-options "-ghci-script=$script"
}
else {
  # Multi-Ziel-REPL: Low-Level, keine Komfort-Imports
  Write-Host "Hinweis: Mehrere Targets starten die experimentelle Cabal-Multi-REPL." -ForegroundColor Yellow
  Write-Host "Auto-Imports aus .ghci-all sind hier absichtlich deaktiviert." -ForegroundColor Yellow
  Write-Host "Nutzen Sie 'import Geometry.Vector' bzw. 'import NumLang.NumLang' manuell." -ForegroundColor Yellow
  cabal repl @Args --repl-options "-ghci-script=$rootGhci"
}
