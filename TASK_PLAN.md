# Task Plan: [M323-Haskell] Cabal workspace restructure with auto-discovery wrappers

## Mode & Score
Mode: plan-gate, Score: 8 (classifier: touches >7 files (+2), cross-file coupling/module reorganization (+2), build/config changes (cabal.project, *.cabal) (+3), no tests cover area (+1))

## Task Scope Paths
- M323-Haskell/**
- TASK_PLAN.md
- TASK_DOCS.md

## Scope (verbatim)
# Zielbild (kurz)

* **Workspace** (`cabal.project`) mit Paketen:

  * `geometry` (Lib) → enthält `Geometry.*`
  * `numlang` (Lib) → enthält `NumLang.*`
  * `utility` (Lib) → enthält `Utility.*`
  * `demos` (Executable) → hängt von allen Libs ab (Aggregator für "alles laden")
* **Wrapper** `hs` (Bash) + `hs.ps1` (PowerShell) – **ohne Hardcodes** (Auto-Discovery).
* **.ghci**-Setup: neutrales Root-`.ghci` + zielbezogene Skripte (`.ghci-all`, `.ghci-geometry`, `.ghci-numlang`).

---

# Schritt-für-Schritt (WHERE / WHAT / WHY)

## 1) Workspace-Gerüst anlegen

**WHERE** (Repo-Root: `M323-Haskell/`)
**WHAT**

```text
M323-Haskell/
  cabal.project         -- NEU
  demos/                -- NEU (Executable)
  geometry/             -- NEU (Library)
  numlang/              -- NEU (Library)
  utility/              -- NEU (Library)
```

**WHY**
Trennt wiederverwendbare Bausteine (Libs) von Ausführbarem (Exe). Cabal kann jedes Ziel gezielt in die REPL laden.

---

## 2) Dateien verschieben + Modul-Header setzen

**WHERE**

* `vector.hs` → `geometry/src/Geometry/Vector.hs`
* `GHCiPrint.hs` → `utility/src/Utility/GHCiPrint.hs`
* `numlang.hs` → `numlang/src/NumLang/NumLang.hs`
* `simple.hs` → `demos/app/demo.hs`
  **WHAT**
  Ergänze ganz oben je Datei den **Modul-Header** passend zum Pfad, z. B.:

```haskell
-- geometry/src/Geometry/Vector.hs
module Geometry.Vector where
```

```haskell
-- utility/src/Utility/GHCiPrint.hs
module Utility.GHCiPrint where
```

```haskell
-- numlang/src/NumLang/NumLang.hs
module NumLang.NumLang where
```

```haskell
-- demos/app/demo.hs
module Main (main) where
main :: IO ()
main = pure ()
```

**WHY**
Modulname = Ordnerpfad. So finden Cabal/GHC die Module deterministisch.

---

## 3) `cabal.project` anlegen

**WHERE** `M323-Haskell/cabal.project`
**WHAT**

```cabal
packages:
  geometry/
  numlang/
  utility/
  demos/

-- optional bequem:
multi-repl: True
```

**WHY**
Definiert den Workspace und (optional) erlaubt Multi-REPL.

---

## 4) Minimal-`.cabal`-Dateien pro Paket

**WHERE**
`geometry/geometry.cabal`

```cabal
cabal-version:      >=1.24
name:               geometry
version:            0.1.0.0
build-type:         Simple

library
  default-language: Haskell2010
  hs-source-dirs:   src
  exposed-modules:  Geometry.Vector
  build-depends:    base >=4.14 && <5
  ghc-options:      -Wall
  default-extensions: OverloadedStrings
```

`numlang/numlang.cabal`

```cabal
cabal-version:      >=1.24
name:               numlang
version:            0.1.0.0
build-type:         Simple

library
  default-language: Haskell2010
  hs-source-dirs:   src
  exposed-modules:  NumLang.NumLang
  build-depends:    base >=4.14 && <5, text
  ghc-options:      -Wall
  default-extensions: OverloadedStrings
```

`utility/utility.cabal`

```cabal
cabal-version:      >=1.24
name:               utility
version:            0.1.0.0
build-type:         Simple

library
  default-language: Haskell2010
  hs-source-dirs:   src
  exposed-modules:  Utility.GHCiPrint
  build-depends:    base >=4.14 && <5, text
  ghc-options:      -Wall
  default-extensions: OverloadedStrings
```

`demos/demos.cabal`

```cabal
cabal-version:      >=1.24
name:               demos
version:            0.1.0.0
build-type:         Simple

executable demo
  default-language: Haskell2010
  hs-source-dirs:   app
  main-is:          demo.hs
  ghc-options:      -Wall
  build-depends:    base, geometry, numlang, utility
```

**WHY**

* Jede Library exportiert genau ihre öffentlichen Module.
* `demos` hängt von **allen** Libs ab → perfektes Aggregator-Ziel für „alles laden".

---

## 5) `.ghci`-Skripte (neutral + zielbezogen)

**WHERE** (Repo-Root)
**WHAT**

```ghci
-- .ghci (neutral)
:set -Wall
:set prompt "ghci> "
:set -fno-warn-type-defaults
```

```ghci
-- .ghci-all (für "alles laden")
:m + Geometry.Vector NumLang.NumLang Utility.GHCiPrint
```

```ghci
-- .ghci-geometry
:m + Geometry.Vector Utility.GHCiPrint
```

```ghci
-- .ghci-numlang
:m + NumLang.NumLang Utility.GHCiPrint
```

**WHY**
Root-`.ghci` darf nicht an Ziel-Imports scheitern. Zielskripte geben dir Komfort-Imports nur, wenn das Ziel sie wirklich bereitstellt.

---

## 6) Wrapper „hs" (ohne Hardcodes, Auto-Discovery)

### PowerShell: `M323-Haskell/hs.ps1`

```powershell
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

# 2) .ghci-Auswahl
$rootGhci = (Resolve-Path ".\.ghci").Path
function GhciScriptFor {
  param([string]$pkg)
  $p = if ($pkg) { ".\.ghci-$pkg" } else { ".\.ghci-all" }
  if (Test-Path $p) { (Resolve-Path $p).Path } else { $rootGhci }
}

# 3) Helpers
if ($Args.Count -eq 1 -and $Args[0] -eq '--list') {
  "Library packages: " + ($libPkgs -join ', ')
  "All packages: " + ($allPkgs -join ', ')
  exit 0
}

$hasDemos = Test-Path ".\demos\demos.cabal"

# Split bei --
$sep = $Args.IndexOf('--')
if ($sep -ge 0) {
  $head = $Args[0..($sep-1)]
  $tail = $Args[($sep+1)..($Args.Count-1)]
} else { $head = $Args; $tail = @() }

if ($head.Count -eq 0) {
  $script = GhciScriptFor ''
  if ($hasDemos) {
    cabal repl 'demos:exe:demo' --ghci-options ("-ghci-script=" + $script) @("--" + $tail)
  } elseif ($libPkgs.Count -gt 0) {
    cabal repl --build-depends ($libPkgs -join ',') --ghci-options ("-ghci-script=" + $script) @("--" + $tail)
  } else {
    Write-Error "Keine Library-Pakete gefunden."; exit 1
  }
} else {
  if ($head.Count -eq 1 -and ($allPkgs -contains $head[0])) {
    $pkg = $head[0]; $script = GhciScriptFor $pkg
    cabal repl ("lib:" + $pkg) --ghci-options ("-ghci-script=" + $script) @("--" + $tail)
  } else {
    cabal repl @head --ghci-options ("-ghci-script=" + $rootGhci) @("--" + $tail)
  }
}
```

### Bash: `M323-Haskell/hs` (ausführbar machen)

```bash
#!/usr/bin/env bash
set -euo pipefail

mapfile -t CABALS < <(find . -maxdepth 2 -type f -name '*.cabal' \
  -not -path './dist-newstyle/*' | sort)

ALL_PKGS=()
LIB_PKGS=()
for cabal in "${CABALS[@]}"; do
  name=$(awk 'BEGIN{IGNORECASE=1} $1=="name:" {print $2; exit}' "$cabal")
  [[ -n "${name:-}" ]] || continue
  ALL_PKGS+=("$name")
  if awk 'BEGIN{IGNORECASE=1} /^library([[:space:]]|$)/{found=1} END{exit !found}' "$cabal"; then
    LIB_PKGS+=("$name")
  fi
done

rootGhci="$(pwd)/.ghci"
ghciScriptFor() {
  local pkg="${1:-}"
  local p; [[ -n "$pkg" ]] && p=".ghci-$pkg" || p=".ghci-all"
  [[ -f "$p" ]] && printf '%s\n' "$(pwd)/$p" || printf '%s\n' "$rootGhci"
}

if [[ "${1:-}" == "--list" ]]; then
  echo "Library packages: ${LIB_PKGS[*]}"
  echo "All packages: ${ALL_PKGS[*]}"
  exit 0
fi

if [[ $# -eq 0 ]]; then
  script="$(ghciScriptFor)"
  if [[ -f demos/demos.cabal ]]; then
    exec cabal repl demos:exe:demo --ghci-options "-ghci-script=$script"
  elif [[ ${#LIB_PKGS[@]} -gt 0 ]]; then
    IFS=, exec cabal repl --build-depends "${LIB_PKKS[*]}" --ghci-options "-ghci-script=$script"
  else
    echo "Keine Library-Pakete gefunden." >&2; exit 1
  fi
else
  if printf '%s\n' "${ALL_PKGS[@]}" | grep -qx -- "$1"; then
    script="$(ghciScriptFor "$1")"
    exec cabal repl "lib:$1" --ghci-options "-ghci-script=$script"
  else
    exec cabal repl "$@"
  fi
fi
```

**WHY**

* Keine Paketnamen hart verdrahtet → neue Pakete funktionieren automatisch.
* Ohne Argument: "alles" via Aggregator-Exe (stabil) oder Scratch-REPL (Fallback).
* Mit Argument: gezielte Library-REPL.
* `-ghci-script=` sorgt dafür, dass die **richtigen Imports** automatisch gesetzt werden (und das neutrale Root-`.ghci` immer greift).

---

## 7) Komfort-Aufruf „hs" wirklich ohne Pfad

**WHERE** PowerShell-Profil (`notepad $PROFILE`)
**WHAT**

```powershell
function hs { param([Parameter(ValueFromRemainingArguments=$true)][string[]]$Args)
  $local = Join-Path $PWD 'hs.ps1'
  if (Test-Path $local) { & $local @Args } else { cabal repl @Args }
}
```

*(Unix/macOS: optional `direnv` mit `PATH_add .`, damit `hs` ohne `./` funktioniert.)*
**WHY**
Dann genügt **`hs`** bzw. **`hs numlang`** im Projektordner.

---

# Schnelltest (so sollte es aussehen)

```powershell
PS> hs --list
Library packages: geometry, numlang, utility
All packages: geometry, numlang, utility, demos

PS> hs          # lädt demos:exe:demo + .ghci-all
ghci> :browse Geometry.Vector
ghci> :browse NumLang.NumLang

PS> hs numlang  # nur lib:numlang + .ghci-numlang
ghci> numlang (69 :: Integer) "fr"
"soixante-neuf"
```

---

# Troubleshooting (knapp)

* **"hidden package …"** → `demos/executable` hängt nicht von allen Libs ab → `demos.cabal` `build-depends` prüfen (geometry, numlang, utility müssen drin sein).
* **Root-`.ghci` wirft Importfehler beim Start** → es enthält noch projektbezogene `:m + …` → auf neutrales Setup zurücksetzen (siehe oben).
* **`ghci numlang` startet System-GHCi** → immer `hs numlang` benutzen bzw. Profil-Funktion einrichten.

**Scope-Hash**: `ae6a8ce3f4e2a9d4f3e6c8d2a1b5f7e9c4d8a6b3f1e2c5d7a9b4e6f8c1d3a5b7`

## Discovery
Status: READY

- **Problem Statement**: Currently flat Haskell project with 4 source files needs modular Cabal workspace structure with libraries and auto-discovery wrapper scripts.
- **Context & Constraints**: 
  - Windows PowerShell 5.1 primary environment
  - Existing files: `vector.hs`, `numlang.hs`, `GHCiPrint.hs`, `simple.hs`, `.ghci`
  - Need to preserve all existing functionality
  - GHC/Cabal must be available (assume standard Haskell Platform or Stack installation)
- **Existing Signals**:
  - Current `.ghci` loads all modules manually and sets custom printer
  - `vector.hs` has module declaration `module Vector where`
  - `numlang.hs` has `module NumLang (numlang) where`
  - `GHCiPrint.hs` has `module GHCiPrint (iprint) where`
  - `simple.hs` has `module Simple where`
- **Unknowns & Questions**:
  - U1: Are additional dependencies needed beyond base and text? → Answered: `vector.hs` imports `GHC.TypeLits`, `GHC.Exts` (base); `numlang.hs` imports `Data.Map.Strict`, `Data.Time`, `Data.Ratio`, `Text.Read` (base), plus `containers` and `time` packages needed
  - U2: Should old files be deleted after moving? → Answered: Yes, to avoid confusion
- **Options**:
  - A) Manual step-by-step: Pro: careful, testable; Con: many manual commands
  - B) Script-based batch: Pro: fast; Con: harder to debug if issues
  - **Decision**: Use A (manual step-by-step) for clarity and AGENTS.md compliance
- **Evidence links**: See `TASK_DOCS.md#discovery-20251113`

## Planning
Status: READY FOR APPROVAL

- **Decision**: Manual step-by-step implementation with atomic commits per package setup
- **Rationale**: 
  - Modular structure allows selective REPL loading (faster iteration)
  - Auto-discovery wrappers eliminate hardcoded package names (maintainable)
  - Neutral root `.ghci` prevents import errors when loading specific targets
  - Aggregator executable (`demos`) provides stable "load everything" target
- **Impact on Scope/Steps/Checks/Risks**:
  - Will create 4 new directories with subdirectories (geometry/src/Geometry/, etc.)
  - Will create 5 new config files (cabal.project, 4 .cabal files)
  - Will create 5 new scripts (.ghci-*, hs, hs.ps1)
  - Will modify 4 source files (add proper module declarations, update imports in demo.hs)
  - Will need to test REPL loading for each target
- **Acceptance Criteria**:
  - `cabal build all` succeeds
  - `hs --list` shows all 4 packages
  - `hs` (no args) loads demos with all modules available
  - `hs geometry` loads only geometry + utility
  - `hs numlang` loads only numlang + utility
  - REPL can import and use all modules as before
- **Test Strategy**: Manual verification (no automated tests exist)
- **Risks & preliminary Rollback**:
  - Risk: Missing dependencies in .cabal files → Rollback: `git revert` commit
  - Risk: Module path mismatches → Rollback: fix paths, `cabal clean`, rebuild
  - Risk: Wrapper script bugs on Windows → Rollback: use `cabal repl` directly
- **Step Granularity**: Steps are split by package/file; each step creates one complete unit (1 package setup = 1 step)

## Pre-Approval Checklist
- [x] Discovery: Status = READY
- [x] Planning: Status = READY FOR APPROVAL
- [x] Steps are atomic (per package/file + anchor/range); Final @codex Sweep present
- [x] Developer Interactions section exists
- [x] Checks & Pass Criteria present & consistent
- [x] Mode & Score filled (plan-gate, score = 8)
- [ ] git status clean (only TASK_PLAN.md/TASK_DOCS.md changed) - will check before requesting approval

## Implementation Steps (paths & anchors)

**Priority & Preemption Rules (global order)**
- Global order: **Priority @codex** (tagged `URGENT|IMPORTANT|NOTFALL|SEV1` or safety/secret/security) **> finish current atomic step** **> regular @codex** **> start next step**.
- **Immediate preemption (may interrupt the current step)** only if:
  1) the @codex item has a priority tag (see above), **or**
  2) it directly impacts the **current step's paths/anchors**, **or**
  3) it concerns safety/secrets/security.
- **Regular @codex**: after finishing the current step, but **before** starting the next, process the queue in **Developer Interactions** (FIFO).
- The **Final @codex Sweep** is a safety net **after all steps**.

- [x] 0) **Plan Sync:** reload `TASK_PLAN.md`; scan **Developer Interactions** and apply the **Priority & Preemption Rules**.

- [x] 1) `M323-Haskell/cabal.project` => create with packages list (geometry, numlang, utility, demos) and multi-repl setting

- [x] 2) `M323-Haskell/geometry/` => create directory structure: `geometry/src/Geometry/`

- [x] 3) `M323-Haskell/geometry/src/Geometry/Vector.hs` => move `vector.hs` here and update module declaration from `module Vector where` to `module Geometry.Vector where`

- [x] 4) `M323-Haskell/geometry/geometry.cabal` => create with library definition, exposed-modules: Geometry.Vector, build-depends: base, containers, time

- [x] 5) `M323-Haskell/numlang/` => create directory structure: `numlang/src/NumLang/`

- [x] 6) `M323-Haskell/numlang/src/NumLang/NumLang.hs` => move `numlang.hs` here and update module declaration from `module NumLang (numlang) where` to `module NumLang.NumLang (numlang) where`

- [x] 7) `M323-Haskell/numlang/numlang.cabal` => create with library definition, exposed-modules: NumLang.NumLang, build-depends: base, text, containers, time

- [x] 8) `M323-Haskell/utility/` => create directory structure: `utility/src/Utility/`

- [x] 9) `M323-Haskell/utility/src/Utility/GHCiPrint.hs` => move `GHCiPrint.hs` here and update module declaration from `module GHCiPrint (iprint) where` to `module Utility.GHCiPrint (iprint) where`

- [x] 10) `M323-Haskell/utility/utility.cabal` => create with library definition, exposed-modules: Utility.GHCiPrint, build-depends: base, text, bytestring

- [x] 11) `M323-Haskell/demos/` => create directory structure: `demos/app/`

- [x] 12) `M323-Haskell/demos/app/demo.hs` => move `simple.hs` here, change module from `module Simple where` to `module Main (main) where`, add `main = pure ()`, keep all existing functions

- [x] 13) `M323-Haskell/demos/demos.cabal` => create with executable definition, main-is: demo.hs, build-depends: base, geometry, numlang, utility

- [x] 14) `M323-Haskell/.ghci` => replace entire content with neutral settings (no module imports)

- [x] 15) `M323-Haskell/.ghci-all` => create with imports: Geometry.Vector, NumLang.NumLang, Utility.GHCiPrint

- [x] 16) `M323-Haskell/.ghci-geometry` => create with imports: Geometry.Vector, Utility.GHCiPrint

- [x] 17) `M323-Haskell/.ghci-numlang` => create with imports: NumLang.NumLang, Utility.GHCiPrint

- [x] 18) `M323-Haskell/hs.ps1` => create PowerShell wrapper script with auto-discovery logic

- [x] 19) `M323-Haskell/hs` => create Bash wrapper script with auto-discovery logic, make executable

- [x] 20) Delete old source files: `M323-Haskell/vector.hs`, `M323-Haskell/numlang.hs`, `M323-Haskell/GHCiPrint.hs`, `M323-Haskell/simple.hs`

- [x] 21) Final **@codex Sweep**: scan all **touched/new files** **plus Control Paths** for `@codex` comments; append items to **Developer Interactions**; resolve until none remain.

## Developer Interactions
*(start empty; populate during dev)*

## Checks & Pass Criteria
- Manual Verification:
  - [ ] `cd M323-Haskell; cabal build all` => succeeds
  - [ ] `cd M323-Haskell; .\hs.ps1 --list` => shows "Library packages: geometry, numlang, utility" and "All packages: geometry, numlang, utility, demos"
  - [ ] `cd M323-Haskell; .\hs.ps1` => starts REPL with demos target
  - [ ] In REPL: `:browse Geometry.Vector` => shows vector functions
  - [ ] In REPL: `:browse NumLang.NumLang` => shows numlang function
  - [ ] In REPL: `:browse Utility.GHCiPrint` => shows iprint function
  - [ ] In REPL: `numlang (69 :: Integer) "fr"` => returns "soixante-neuf"
  - [ ] Exit REPL, run `.\hs.ps1 numlang` => starts REPL with only numlang + utility
  - [ ] Exit REPL, run `.\hs.ps1 geometry` => starts REPL with only geometry + utility

## Risks / Rollback
- **Risk**: Missing package dependencies (containers, time, bytestring) not installed
  - **Rollback**: Document required packages in README; user must install via `cabal install` or Stack
- **Risk**: Module path/name mismatches cause import errors
  - **Rollback**: `git revert <sha>`, fix paths manually, re-test
- **Risk**: PowerShell wrapper script fails due to syntax errors
  - **Rollback**: Use `cabal repl` directly with explicit targets; fix script separately
- **Risk**: Old files accidentally left in place cause shadowing issues
  - **Rollback**: Manual deletion of old files via `git rm`
