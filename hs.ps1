<#
    hs.ps1
    ----------
    Usage examples (from M323-Haskell root):

      pwsh .\hs.ps1                # load ALL packages from cabal.project (except startup)
      pwsh .\hs.ps1 geometry       # only geometry
      pwsh .\hs.ps1 geometry numlang utility

    If you want "hs geometry numlang":
      - Put this repo on your PATH, or
      - Define an alias in your PS session:
            Set-Alias hs "$PWD\hs.ps1"
#>

[CmdletBinding()]
param(
    [Parameter(ValueFromRemainingArguments = $true)]
    [string[]] $Packages
)

$ErrorActionPreference = "Stop"

function Write-Info {
    param([string]$Message)
    Write-Host "[INFO] $Message" -ForegroundColor Cyan
}

function Write-Warn {
    param([string]$Message)
    Write-Warning $Message
}

function Write-Err {
    param([string]$Message)
    Write-Host "[ERROR] $Message" -ForegroundColor Red
}

#---------------------------
# Helper: read packages from cabal.project
#---------------------------
function Get-CabalProjectPackages {
    param(
        [Parameter(Mandatory)]
        [string] $CabalProjectPath
    )

    if (-not (Test-Path $CabalProjectPath)) {
        throw "cabal.project not found at '$CabalProjectPath'. Run this script from the project root."
    }

    $lines = Get-Content -LiteralPath $CabalProjectPath

    $knownPkgs = New-Object System.Collections.Generic.List[string]
    $inPackagesBlock = $false

    foreach ($line in $lines) {
        $trim = $line.Trim()

        # Skip pure comment lines
        if ($trim -like "--*") { continue }

        if (-not $inPackagesBlock) {
            # Start of packages: block?
            if ($trim -like "packages:*") {
                $inPackagesBlock = $true
            }
            continue
        }
        else {
            # In packages block: read indented entries, stop on blank or new top-level field
            if ($trim -eq "") { break }

            # If the line is not indented -> probably a new top-level field (e.g. multi-repl)
            if ($line -match '^[^\s]') { break }

            # Now we have something like "  geometry/" or "  ./geometry/"
            $pkgEntry = $trim

            # Kommentare nach "--" entfernen
            $commentIndex = $pkgEntry.IndexOf("--")
            if ($commentIndex -ge 0) {
                $pkgEntry = $pkgEntry.Substring(0, $commentIndex)
            }
            $pkgEntry = $pkgEntry.Trim()

            if ($pkgEntry -eq "") { continue }

            # take first token (in case someone wrote "geometry/  -- comment")
            $pkgEntry = $pkgEntry.Split()[0]

            # remove ./ prefix
            if ($pkgEntry.StartsWith("./")) {
                $pkgEntry = $pkgEntry.Substring(2)
            }

            # remove trailing slash if present
            if ($pkgEntry.EndsWith("/")) {
                $pkgEntry = $pkgEntry.Substring(0, $pkgEntry.Length - 1)
            }

            if (-not [string]::IsNullOrWhiteSpace($pkgEntry)) {
                $knownPkgs.Add($pkgEntry)
            }
        }
    }

    return $knownPkgs.ToArray()
}

#---------------------------
# Helper: find <pkg>.cabal file
#---------------------------
function Find-CabalFileForPackage {
    param(
        [Parameter(Mandatory)]
        [string] $ProjectRoot,

        [Parameter(Mandatory)]
        [string] $PackageName
    )

    # Prefer standard layout <root>\<pkg>\<pkg>.cabal
    $expected = Join-Path (Join-Path $ProjectRoot $PackageName) ("$PackageName.cabal")
    if (Test-Path $expected) {
        return (Resolve-Path $expected).Path
    }

    # Fallback: search recursively, but ignore dist-newstyle to avoid duplicates
    $pattern = "$PackageName.cabal"
    $results = Get-ChildItem -Path $ProjectRoot -Recurse -File -Filter $pattern |
               Where-Object { $_.FullName -notmatch "\\dist-newstyle\\" }

    if ($results.Count -eq 0) {
        Write-Warn "[WARN] Kein .cabal-File für Package '$PackageName' gefunden -> Paket ignoriert"
        return $null
    }
    elseif ($results.Count -gt 1) {
        Write-Warn ("[WARN] Mehrere .cabal-Files für Package '{0}' gefunden:`n  {1} -> Paket ignoriert" -f `
            $PackageName, ($results.FullName -join "`n  "))
        return $null
    }
    else {
        return $results[0].FullName
    }
}

#---------------------------
# Helper: parse exposed-modules out of library stanza in .cabal
#---------------------------
function Get-ExposedModulesFromCabalFile {
    param(
        [Parameter(Mandatory)]
        [string] $CabalFilePath
    )

    $lines = Get-Content -LiteralPath $CabalFilePath
    $modules = New-Object System.Collections.Generic.List[string]

    $inLibrary = $false

    for ($i = 0; $i -lt $lines.Count; $i++) {
        $line = $lines[$i]
        $trim = $line.Trim()

        # Skip full comment lines
        if ($trim -like "--*") { continue }

        # detect library stanza (column 1 "library")
        if (-not $inLibrary -and $line -match '^\s*library\b') {
            $inLibrary = $true
            continue
        }

        if ($inLibrary) {
            # If we hit another top-level stanza (non-indented, non-empty), library block ended
            if ($trim -ne "" -and $line -match '^[^\s]') {
                # e.g. "executable foo", "test-suite bar"
                $inLibrary = $false
                continue
            }

            # Find "exposed-modules:" field
            if ($line -match '^\s*exposed-modules\s*:(.*)$') {
                # Rest of current line after ":"
                $rest = $matches[1].Trim()
                if ($rest -ne "") {
                    $rest.Split() | ForEach-Object {
                        if (-not [string]::IsNullOrWhiteSpace($_)) {
                            $modules.Add($_)
                        }
                    }
                }

                # Now consume subsequent continuation lines:
                #  - still indented
                #  - do NOT contain a ":" (otherwise it's a new field)
                $j = $i + 1
                while ($j -lt $lines.Count) {
                    $nextLine = $lines[$j]
                    $nextTrim = $nextLine.Trim()

                    # Stop on blank, comment, or de-indented top-level
                    if ($nextTrim -eq "") { break }
                    if ($nextTrim -like "--*") { $j++; continue }

                    # Require indentation
                    if ($nextLine -match '^[^\s]') { break }

                    # New field? (contains ":")
                    if ($nextLine -match ':\s*') { break }

                    # Otherwise treat as continuation with module names
                    $nextTrim.Split() | ForEach-Object {
                        if (-not [string]::IsNullOrWhiteSpace($_)) {
                            $modules.Add($_)
                        }
                    }

                    $j++
                }

                # Move main loop index
                $i = $j - 1
            }
        }
    }

    return $modules.ToArray()
}

#---------------------------
# Helper: write Startup.Startup.hs
#---------------------------
function Write-StartupModule {
    param(
        [Parameter(Mandatory)]
        [string[]] $ModuleNames,

        [Parameter(Mandatory)]
        [string] $ProjectRoot
    )

    if ($ModuleNames.Count -eq 0) {
        throw "Write-StartupModule called with empty module list."
    }

    $startupDir = Join-Path $ProjectRoot "startup\src\Startup"
    if (-not (Test-Path $startupDir)) {
        New-Item -ItemType Directory -Path $startupDir -Force | Out-Null
    }

    $startupFile = Join-Path $startupDir "Startup.hs"

    $lines = New-Object System.Collections.Generic.List[string]

    $lines.Add("module Startup.Startup")
    $lines.Add("  ( module $($ModuleNames[0])")

    for ($i = 1; $i -lt $ModuleNames.Count; $i++) {
        $lines.Add("  , module $($ModuleNames[$i])")
    }

    $lines.Add("  ) where")
    $lines.Add("")

    foreach ($m in $ModuleNames) {
        $lines.Add("import $m")
    }

    Write-Info "Generiere '$startupFile' mit $($ModuleNames.Count) Modulen ..."
    $lines | Set-Content -LiteralPath $startupFile -Encoding UTF8
}

#---------------------------
# Helper: ensure .ghci imports Startup.Startup (optional sugar)
#---------------------------
function Ensure-GhciConfig {
    param(
        [Parameter(Mandatory)]
        [string] $ProjectRoot
    )

    $ghciPath = Join-Path $ProjectRoot ".ghci"
    $importLine = "import Startup.Startup"

    if (-not (Test-Path $ghciPath)) {
        Write-Info "Erzeuge '.ghci' mit 'import Startup.Startup' ..."
        $importLine | Set-Content -LiteralPath $ghciPath -Encoding UTF8
        return
    }

    $existing = Get-Content -LiteralPath $ghciPath
    if (-not ($existing -contains $importLine)) {
        Write-Info "Ergänze '.ghci' um 'import Startup.Startup' ..."
        Add-Content -LiteralPath $ghciPath -Encoding UTF8 -Value $importLine
    }
}

#---------------------------
# Helper: start cabal repl startup
#---------------------------
function Invoke-CabalReplStartup {
    param(
        [Parameter(Mandatory)]
        [string] $ProjectRoot
    )

    Write-Info "Starte 'cabal repl startup' ..."
    Push-Location $ProjectRoot
    try {
        & cabal repl startup
    }
    finally {
        Pop-Location
    }
}

#===========================
# MAIN
#===========================

$projectRoot = (Get-Location).ProviderPath
$cabalProjectPath = Join-Path $projectRoot "cabal.project"

# Registrierte Packages laden (aber noch nichts ausgeben)
$knownPkgs = Get-CabalProjectPackages -CabalProjectPath $cabalProjectPath

# 1) Argument-Normalisierung:
#    ./numlang, ./numlang/, .\numlang, .\numlang\  ->  numlang
if ($Packages -and $Packages.Count -gt 0) {
    $normalized = New-Object System.Collections.Generic.List[string]
    foreach ($p in $Packages) {
        if ($p -match '^\.[/\\](.+?)[/\\]?$') {
            # Tab-Completion-Pfad (oder manuell getippter relativer Pfad)
            $normalized.Add($matches[1])
        }
        else {
            $normalized.Add($p)
        }
    }
    $Packages = $normalized.ToArray()
}

# 2) Sonderfälle: --list / -list / list und --help / -help / help
if ($Packages -and $Packages.Count -eq 1) {
    $arg0 = $Packages[0].ToLower()

    switch ($arg0) {
        '--list' { $mode = 'list' }
        '-list'  { $mode = 'list' }
        'list'   { $mode = 'list' }
        '--help' { $mode = 'help' }
        '-help'  { $mode = 'help' }
        'help'   { $mode = 'help' }
        default  { $mode = $null }
    }

    if ($mode -eq 'list') {
        Write-Host "In cabal.project registrierte Packages:"
        foreach ($p in $knownPkgs) {
            Write-Host "  $p"
        }
        exit 0
    }
    elseif ($mode -eq 'help') {
        Write-Host "In cabal.project registrierte Packages:"
        foreach ($p in $knownPkgs) {
            Write-Host "  $p"
        }

        Write-Host ""
        Write-Host "Beispiele:"
        Write-Host "  .\hs.ps1 numlang"
        Write-Host "      # nur numlang laden"
        Write-Host "  .\hs.ps1 geometry numlang"
        Write-Host "      # geometry & numlang laden"
        Write-Host "  .\hs.ps1"
        Write-Host "      # alle registrierten Packages (außer 'startup') laden"
        exit 0
    }
}

# Ab hier der normale Ablauf
Write-Info "Projektroot: $projectRoot"
Write-Info ("In cabal.project registrierte Packages: {0}" -f ($knownPkgs -join ", "))

# If no packages were given on the command line -> use all from cabal.project
if (-not $Packages -or $Packages.Count -eq 0) {
    Write-Info "Keine Packages angegeben -> verwende alle aus cabal.project"
    $requestedPkgsRaw = $knownPkgs
}
else {
    $requestedPkgsRaw = $Packages
}

# Filter against known packages and warn on unknown
$requestedPkgs = New-Object System.Collections.Generic.List[string]

foreach ($pkg in $requestedPkgsRaw) {
    if ($knownPkgs -contains $pkg) {
        if (-not ($requestedPkgs -contains $pkg)) {
            $requestedPkgs.Add($pkg)
        }
    }
    else {
        Write-Warn "[WARN] Package '$pkg' nicht in cabal.project -> ignoriert"
    }
}

# Remove startup to avoid cycles
$requestedPkgs = $requestedPkgs | Where-Object { $_ -ne "startup" }

if ($requestedPkgs.Count -eq 0) {
    Write-Err "Keine gültigen Packages angegeben (nach Filterung). Abbruch."
    exit 1
}

Write-Info ("Gewählte Packages: {0}" -f ($requestedPkgs -join ", "))

# Map: pkg -> cabal file
$cabalFileByPkg = @{}

foreach ($pkg in $requestedPkgs) {
    $cabalPath = Find-CabalFileForPackage -ProjectRoot $projectRoot -PackageName $pkg
    if ($null -ne $cabalPath) {
        $cabalFileByPkg[$pkg] = $cabalPath
    }
}

if ($cabalFileByPkg.Keys.Count -eq 0) {
    Write-Err "Keine .cabal-Files für angegebene Packages gefunden. Abbruch."
    exit 1
}

# Map: pkg -> modules[]
$modulesByPkg = @{}

foreach ($pkg in $requestedPkgs) {
    if (-not $cabalFileByPkg.ContainsKey($pkg)) { continue }

    $cabalPath = $cabalFileByPkg[$pkg]
    Write-Info "Lese 'exposed-modules' aus '$cabalPath' ..."
    $mods = Get-ExposedModulesFromCabalFile -CabalFilePath $cabalPath

    if (-not $mods -or $mods.Count -eq 0) {
        Write-Warn "[WARN] Keine 'exposed-modules' im Package '$pkg' gefunden."
        $modulesByPkg[$pkg] = @()
    }
    else {
        Write-Info ("Package '{0}': {1} Modul(e): {2}" -f $pkg, $mods.Count, ($mods -join ", "))
        $modulesByPkg[$pkg] = $mods
    }
}

# Aggregate & deduplicate modules in the order of requested packages
$allModules = New-Object System.Collections.Generic.List[string]

foreach ($pkg in $requestedPkgs) {
    if ($modulesByPkg.ContainsKey($pkg)) {
        foreach ($m in $modulesByPkg[$pkg]) {
            if (-not ($allModules -contains $m)) {
                $allModules.Add($m)
            }
        }
    }
}

if ($allModules.Count -eq 0) {
    Write-Err "Keine Module gefunden (exposed-modules leer?). Abbruch."
    exit 1
}

Write-Info ("Gesamte Modul-Liste: {0}" -f ($allModules -join ", "))

# Generate Startup.Startup.hs
Write-StartupModule -ModuleNames $allModules.ToArray() -ProjectRoot $projectRoot

# Optional: ensure .ghci imports Startup.Startup
Ensure-GhciConfig -ProjectRoot $projectRoot

# Start cabal repl
Invoke-CabalReplStartup -ProjectRoot $projectRoot
