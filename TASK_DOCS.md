# Task Docs: [M323-Haskell] Cabal workspace restructure

## Mode & Score
Mode: plan-gate, Score: 8 (factors: touches >7 files, cross-file coupling, build/config changes, no tests)

## Discovery (2025-11-13)

### Current State Analysis
- **Existing files**:
  - `vector.hs`: 2D vector operations with type-level polymorphism; module `Vector`; imports from base (GHC.TypeLits, GHC.Exts)
  - `numlang.hs`: Multi-language number-to-words converter; module `NumLang (numlang)`; imports from base (Data.Map.Strict, Data.Time, Data.Ratio, Text.Read) plus text, containers, time packages
  - `GHCiPrint.hs`: Custom REPL printer for raw string output; module `GHCiPrint (iprint)`; imports from base, text, bytestring
  - `simple.hs`: Basic arithmetic and pattern-matching examples; module `Simple`
  - `.ghci`: Custom REPL setup that loads all 4 modules and sets custom printer

### Dependency Analysis
From import inspection:
- **base** (standard): all files
- **text**: `numlang.hs`, `GHCiPrint.hs`
- **containers**: `numlang.hs` (Data.Map.Strict)
- **time**: `numlang.hs` (Data.Time)
- **bytestring**: `GHCiPrint.hs`

### Module Naming Strategy
Per Cabal convention:
- `vector.hs` → `Geometry.Vector` (in `geometry/src/Geometry/Vector.hs`)
- `numlang.hs` → `NumLang.NumLang` (in `numlang/src/NumLang/NumLang.hs`)
- `GHCiPrint.hs` → `Utility.GHCiPrint` (in `utility/src/Utility/GHCiPrint.hs`)
- `simple.hs` → `Main` (in `demos/app/demo.hs`) with `main = pure ()`

### Auto-Discovery Requirements
Wrapper scripts must:
1. Find all `*.cabal` files (excluding `dist-newstyle/`)
2. Parse `name:` field from each
3. Detect library vs executable via `library` stanza presence
4. Route to appropriate `.ghci-*` script based on target
5. Handle no-args case via `demos:exe:demo` (aggregator)

## Changes
Created Cabal workspace with 4 packages:

### Directories & Files Created
- `cabal.project` - Workspace configuration with multi-repl enabled
- `geometry/` - Library package
  - `geometry/src/Geometry/Vector.hs` - Moved from `vector.hs`, updated module to `Geometry.Vector`
  - `geometry/geometry.cabal` - Library definition with GADTs, TypeFamilies extensions
- `numlang/` - Library package
  - `numlang/src/NumLang/NumLang.hs` - Moved from `numlang.hs`, updated module to `NumLang.NumLang`
  - `numlang/numlang.cabal` - Library definition with FlexibleInstances, TypeSynonymInstances
- `utility/` - Library package
  - `utility/src/Utility/GHCiPrint.hs` - Moved from `GHCiPrint.hs`, updated module to `Utility.GHCiPrint`
  - `utility/utility.cabal` - Library definition with RankNTypes
- `demos/` - Executable package (aggregator)
  - `demos/app/demo.hs` - Moved from `simple.hs`, changed module to `Main`, added `main = pure ()`
  - `demos/demos.cabal` - Executable depending on all 3 libraries

### Configuration Files
- `.ghci` - Neutral root config (no module imports)
- `.ghci-all` - Imports all modules + custom printer
- `.ghci-geometry` - Imports Geometry.Vector + printer
- `.ghci-numlang` - Imports NumLang.NumLang + printer

### Wrapper Scripts
- `hs.ps1` - PowerShell wrapper with auto-discovery
- `hs` - Bash wrapper with auto-discovery

### Files Deleted
- `vector.hs`, `numlang.hs`, `GHCiPrint.hs`, `simple.hs`

### Key Fixes Applied
- Removed `OverloadedStrings` from numlang.cabal (was causing type ambiguity)
- Added language extensions: GADTs, TypeFamilies, RankNTypes, FlexibleInstances, etc.

## Checks & Results
✅ `cabal build all` - SUCCESS (all packages built)
✅ `.\hs.ps1 --list` - SUCCESS
   Output: `Library packages: geometry, numlang, utility`
           `All packages: demos, geometry, numlang, utility`

## Debugging Session (Post-Implementation)
**Issue**: Wrapper scripts initially showed cabal help text instead of loading REPL when invoked with package name (e.g., `.\hs.ps1 numlang`).

**Root Cause**: Used incorrect flag `--ghci-options` instead of `--repl-options`. Cabal repl documentation shows:
- ✓ `--repl-options="FLAGS"` passes flags to GHCi (space-separated string)
- ✗ `--ghci-options` does not exist for `cabal repl` command

**Fixes Applied**:
1. Changed both wrappers from `--ghci-options "-ghci-script=<path>"` to `--repl-options="-ghci-script <path>"`
2. Changed from absolute paths to relative paths (e.g., `.ghci-numlang` instead of `C:\...\\.ghci-numlang`)
3. Fixed PowerShell argument construction: `@("--repl-options=-ghci-script $script")` as single array element

**Verification Tests**:
```powershell
.\hs.ps1 --list       # ✓ Lists: "Library packages: geometry, numlang, utility"
.\hs.ps1              # ✓ Loads demos:exe:demo with .ghci-all
.\hs.ps1 numlang      # ✓ Loads lib:numlang with .ghci-numlang
.\hs.ps1 geometry     # ✓ Loads lib:geometry with .ghci-geometry
.\hs.ps1 utility      # ✓ Loads lib:utility with .ghci-utility
```

All tests show "Ok, one module loaded." confirming REPL starts correctly with appropriate .ghci script.

## Follow-ups / Risks
- User must have GHC 9.6.7 / Cabal 3.12+ installed (not auto-installed by this task)
- PowerShell wrapper assumes PS 5.1+ (verified working in user's environment)
- Bash wrapper (`hs`) updated with same fixes but not tested on Windows (requires Git Bash/WSL)
