param([Parameter(ValueFromRemainingArguments=$true)][string[]]$Args)

# 1) Cabal-Dateien finden (ohne dist-newstyle)
$cabals = Get-ChildItem -Path . -Recurse -Depth 2 -Filter *.cabal |
  Where-Object { $_.FullName -notmatch '\\dist-newstyle\\' } |
  Sort-Object FullName

$allPkgs = @(); $libPkgs = @()
foreach ($c in $cabals) {
  $name = (Select-String -Path $c.FullName -Pattern '^\s*name:\s*(\S+)' -CaseSensitive:$false |
           ForEach-Object { $_.Matches[0].Groups[1].Value } | Select-Object -First 1)
  if (-not $name) { continue }
  $allPkgs += $name
  if (Select-String -Path $c.FullName -Pattern '^\s*library(\s|$)' -CaseSensitive:$false) {
    $libPkgs += $name
  }
}

# 2) .ghci-Auswahl (use relative paths to avoid quoting issues)
function GhciScriptFor {
  param([string]$pkg)
  $p = if ($pkg) { ".ghci-$pkg" } else { ".ghci-all" }
  if (Test-Path $p) { $p } else { ".ghci" }
}

# 3) Helpers
if ($Args.Count -eq 1 -and $Args[0] -eq '--list') {
  "Library packages: " + ($libPkgs -join ', ')
  "All packages: " + ($allPkgs -join ', ')
  exit 0
}

$hasDemos = Test-Path ".\demos\demos.cabal"

# Split bei -- (separator for cabal repl passthrough args)
$sep = $Args.IndexOf('--')
if ($sep -ge 0) {
  if ($sep -eq 0) {
    $head = @()
    $tail = $Args[1..($Args.Count-1)]
  } else {
    $head = $Args[0..($sep-1)]
    $tail = $Args[($sep+1)..($Args.Count-1)]
  }
} else { 
  $head = $Args
  $tail = @()
}

# Build cabal args with proper -- handling
function BuildCabalArgs {
  param([string[]]$baseArgs, [string]$ghciScript, [string[]]$extraArgs)
  # Use --repl-options to pass flags to GHCi (space-separated string)
  $result = $baseArgs + @("--repl-options=-ghci-script $ghciScript")
  if ($extraArgs.Count -gt 0) {
    $result += '--'
    $result += $extraArgs
  }
  return $result
}

if ($head.Count -eq 0) {
  # No target specified: load demos or all libs
  $script = GhciScriptFor ''
  if ($hasDemos) {
    $cabalArgs = BuildCabalArgs @('repl', 'demos:exe:demo') $script $tail
    & cabal @cabalArgs
  } elseif ($libPkgs.Count -gt 0) {
    $cabalArgs = BuildCabalArgs @('repl', '--build-depends', ($libPkgs -join ',')) $script $tail
    & cabal @cabalArgs
  } else {
    Write-Error "Keine Library-Pakete gefunden."; exit 1
  }
} else {
  # Target specified
  if ($head.Count -eq 1 -and ($allPkgs -contains $head[0])) {
    # Single package name: load as lib
    $pkg = $head[0]
    $script = GhciScriptFor $pkg
    $cabalArgs = BuildCabalArgs @('repl', "lib:$pkg") $script $tail
    & cabal @cabalArgs
  } else {
    # Multiple args or unknown: pass through to cabal
    $script = GhciScriptFor ''
    $cabalArgs = @('repl') + $head + @("--repl-options=-ghci-script $script")
    if ($tail.Count -gt 0) {
      $cabalArgs += '--'
      $cabalArgs += $tail
    }
    & cabal @cabalArgs
  }
}
