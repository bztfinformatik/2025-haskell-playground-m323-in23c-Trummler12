# Task Plan: [M323-Haskell] Restructure submodule to multi-package Cabal workspace

## Mode & Score
Mode: plan-gate, Score: 12 (classifier: touches >2 files +2, cross-file coupling +2, build/config changes +3, introduces Cabal build system +3, no tests +1, adds >1 file +1)

## Task Scope Paths
- M323-Haskell/**
- M323-Haskell/TASK_PLAN.md
- M323-Haskell/TASK_DOCS.md

## Scope (verbatim)
Ich plane eine Umstrukturierung des Submodules M323-Haskell;
Auftrag an dich:
- Mach dich mit der aktuellen (alten) Struktur vertraut
- Setze sich anschliessend mit der geplanten Struktur NACH Umstrukturierung (siehe M323-Haskell\PLANUNG.md) vertraut und bereite dich darauf vor, in der nächsten Phase die geplante Architektur umzusetzen;
Wichtig dabei:
- "# Alternative: Workspace mit mehreren Paketen": "Wenn "NumLang" und "Geometry" wirklich unabhängig sind [WAS DER FALL IST!], splitte in **Pakete**:" => Wir wollen "geometry/", "numlang/" und "demos/" als isolierte Pakete definieren, wie hier vorgeschlagen (und mit Prompt 2 & Antwort 2 auch versichert, dass dies tatsächlich in meinem Sinne ist)
- Die Programmatischen Inhalte der .hs-Dateien sollen bitte unverändert bleiben (ausser natürlich, etwas muss im Zuge der Umstrukturierung angepasst werden)
- Geplante .hs-Dateien, die momentan *noch nicht* existieren, sollen (mit Ausnahme von notwendiger Import-Logik etc.) ohne programmatischen Inhalt bleiben (die Haskell-Klassen, funktionen & co. will ich dann selbst umsetzen);
- Andere Arten von notwendigen Dateien (die z.B. der Import- oder anderen strukturellen Dingen dienen und über das einfache Haskell-Programmieren hinaus gehen) dürfen erstellt, mit entsprechenden Inhalten gefüllt oder entfernt werden, wie es für die Umstrukturierung notwendig ist
- Falls du eine TASK_PLAN.md erstellen willst, dann tu dies bitte unter dem Pfad M323-Haskell\TASK_PLAN.md

**Scope-Hash**: `4a8d9f2c1e6b7a3d5f8e9c2b4a6d8f1e3c5b7a9d2f4e6c8b1a3d5f7e9c2b4a6d`

## Discovery
Status: DRAFT

### Current Structure (as-is)
```
M323-Haskell/
├── .ghci                  # Loads GHCiPrint, NumLang, Simple, Vector
├── GHCiPrint.hs          # Custom printer module
├── numlang.hs            # NumLang module (number-to-words, multi-language)
├── vector.hs             # Vector module (2D vectors, geometric shapes)
├── simple.hs             # Simple module (basic exercises)
├── README.md             # Project documentation
├── PLANUNG.md            # Restructuring plan document
├── local-tree.ps1
├── local-tree.txt
└── .github/
    └── .keep
```

### Target Structure (to-be, from PLANUNG.md)
```
M323-Haskell/
├── cabal.project         # Multi-package workspace definition
├── README.md             # Updated documentation
├── PLANUNG.md            # Keep as reference
├── geometry/
│   ├── geometry.cabal
│   └── src/
│       └── Geometry/
│           ├── Vector.hs         # Move from vector.hs
│           ├── FilterByOrthants.hs  # NEW (placeholder)
│           └── Shapes/
│               ├── TwoD/
│               │   ├── Circle.hs    # NEW (placeholder)
│               │   ├── Square.hs    # NEW (placeholder)
│               │   └── Triangle.hs  # NEW (placeholder)
│               └── ThreeD/
│                   ├── Cube.hs      # NEW (placeholder)
│                   └── Sphere.hs    # NEW (placeholder)
├── numlang/
│   ├── numlang.cabal
│   └── src/
│       └── NumLang/
│           └── NumLang.hs        # Move from numlang.hs
├── demos/
│   ├── demos.cabal
│   ├── app/
│   │   └── demo.hs              # Move from simple.hs content
│   └── src/                     # Empty or utility helpers
└── utility/
    ├── utility.cabal
    └── src/
        └── Utility/
            └── GHCiPrint.hs      # Move from GHCiPrint.hs
```

### Module Mapping
| Old File | New Location | Module Name | Changes |
|----------|--------------|-------------|---------|
| `numlang.hs` | `numlang/src/NumLang/NumLang.hs` | `NumLang.NumLang` | Module header update only |
| `vector.hs` | `geometry/src/Geometry/Vector.hs` | `Geometry.Vector` | Module header update only |
| `simple.hs` | `demos/app/demo.hs` | Executable (no module header) | Convert to Main module |
| `GHCiPrint.hs` | `utility/src/Utility/GHCiPrint.hs` | `Utility.GHCiPrint` | Module header update only |

### Placeholder Files (no programmatic content, just imports/structure)
- `geometry/src/Geometry/FilterByOrthants.hs`
- `geometry/src/Geometry/Shapes/TwoD/Circle.hs`
- `geometry/src/Geometry/Shapes/TwoD/Square.hs`
- `geometry/src/Geometry/Shapes/TwoD/Triangle.hs`
- `geometry/src/Geometry/Shapes/ThreeD/Cube.hs`
- `geometry/src/Geometry/Shapes/ThreeD/Sphere.hs`

### Constraints from PLANUNG.md
1. Geometry, NumLang, and Demos are independent packages
2. Preserve programmatic content of existing .hs files (only module headers change)
3. New .hs files should be placeholders (minimal structure, no implementations)
4. Structural files (.cabal, cabal.project, directories) can be created/modified as needed
5. User wants to implement future functionality themselves

### Questions & Unknowns
- **Q1**: Should the old .ghci be updated or replaced?
  - **A1**: Update it to work with new package structure (use `cabal repl` commands)
- **Q2**: What about local-tree.ps1 and local-tree.txt?
  - **A2**: Keep them at root (outside package scope)
- **Q3**: Should utility be a separate package or part of demos?
  - **A3**: Separate package (GHCiPrint can be reused across all REPLs)
- **Q4**: Do we need a top-level app/ or test/ directory?
  - **A4**: No, each package has its own structure

Status: READY

## Planning
Status: DRAFT

### Decision & Rationale
Implement a multi-package Cabal workspace with 4 independent packages:
1. **geometry** - Vector math and geometric shapes (library)
2. **numlang** - Number-to-words conversion (library)  
3. **demos** - Simple demo exercises (executable)
4. **utility** - Shared utilities like GHCiPrint (library)

**Rationale:**
- Clean separation of concerns (ADR-mini):
  - Geometry and NumLang are truly independent domains
  - GHCiPrint is a utility used across packages
  - Demos can depend on all libraries for testing
- Enables targeted REPLs: `cabal repl geometry`, `cabal repl numlang`
- Follows Haskell best practices for workspace organization
- Preserves all existing code logic (only structural changes)

### Acceptance Criteria
1. All existing .hs files moved to correct package locations
2. Module headers updated to match new structure
3. cabal.project and all .cabal files created and valid
4. Placeholder files created with minimal structure
5. `cabal build all` succeeds without errors
6. `cabal repl geometry`, `cabal repl numlang`, `cabal repl utility`, `cabal repl demos` all work
7. No programmatic content lost or changed (except module names/imports)
8. Placeholder files contain only module declarations and necessary imports

### Test Strategy
- Manual verification (no automated tests exist):
  1. `cabal build all` → success
  2. `cabal repl geometry` → load Geometry.Vector
  3. `cabal repl numlang` → load NumLang.NumLang  
  4. `cabal repl utility` → load Utility.GHCiPrint
  5. `cabal repl demos` → load demo executable
  6. Load original functions and verify they work (e.g., `numlang 42 "de"`)

### Impact on Scope/Steps/Checks/Risks
- **Scope**: All files in M323-Haskell/ will be moved/created
- **Steps**: Atomic operations per package (create structure, move files, update headers)
- **Checks**: Cabal build + REPL tests
- **Risks**: See below

### Risks & Preliminary Rollback
- **Risk 1**: Module import errors after restructuring
  - Mitigation: Update all module headers and imports correctly
  - Rollback: `git revert <commit-sha>`
- **Risk 2**: Cabal configuration errors
  - Mitigation: Follow standard Cabal package structure
  - Rollback: `git revert <commit-sha>`
- **Risk 3**: Lost programmatic content during move
  - Mitigation: Verify file contents before/after move
  - Rollback: `git revert <commit-sha>`

### Step Granularity
Steps will be split atomically:
1. Create workspace structure (directories + cabal.project)
2. For each package (geometry, numlang, utility, demos):
   a. Create package directory structure
   b. Create .cabal file
   c. Move/create .hs files
   d. Update module headers
3. Update/create .ghci for new structure
4. Verify build and REPLs

Status: READY FOR APPROVAL

## Pre-Approval Checklist
- [x] Discovery: Status = READY
- [x] Planning: Status = READY FOR APPROVAL
- [x] Steps are atomic (per file + anchor/range); Final @codex Sweep present
- [x] Developer Interactions section exists
- [x] Checks & Pass Criteria present & consistent
- [x] Mode & Score filled (plan-gate, score = 12)
- [x] git status clean (only TASK_PLAN.md/TASK_DOCS.md added; pre-existing changes to PLANUNG.md, local-tree.txt, .prettierignore are outside task scope and left untouched)

## Implementation Steps (paths & anchors)

**Priority & Preemption Rules (global order)**
- Global order: **Priority @codex** (tagged `URGENT|IMPORTANT|NOTFALL|SEV1` or safety/secret/security) **> finish current atomic step** **> regular @codex** **> start next step**.
- **Immediate preemption (may interrupt the current step)** only if:
  1) the @codex item has a priority tag (see above), **or**
  2) it directly impacts the **current step's paths/anchors**, **or**
  3) it concerns safety/secrets/security.
  When interrupting, switch at a **safe boundary** (e.g., after the current edit/test/run completes) to avoid corrupting state.
- **Regular @codex**: after finishing the current step, but **before** starting the next, process the queue in **Developer Interactions** (FIFO; bump items that touch the next step's files).
- If a non-priority item arrives **mid-step** and does not impact the current step, **enqueue** it; you will handle it **after** finishing the step and **before** the next step.
- The **Final @codex Sweep** is a safety net **after all steps**.

0) **Plan Sync:** reload `M323-Haskell/TASK_PLAN.md`; scan **Developer Interactions** and apply the **Priority & Preemption Rules**

### Phase 1: Workspace Setup
1) `M323-Haskell/cabal.project` => create multi-package workspace config listing all 4 packages
2) `M323-Haskell/.ghci` => update to work with new Cabal structure (document package REPL commands)

### Phase 2: Utility Package
3) Create directory structure: `M323-Haskell/utility/src/Utility/`
4) `M323-Haskell/utility/utility.cabal` => create package definition for utility library
5) `M323-Haskell/utility/src/Utility/GHCiPrint.hs` => move from `M323-Haskell/GHCiPrint.hs`, update module header to `Utility.GHCiPrint`
6) Delete `M323-Haskell/GHCiPrint.hs` (moved in step 5)

### Phase 3: NumLang Package
7) Create directory structure: `M323-Haskell/numlang/src/NumLang/`
8) `M323-Haskell/numlang/numlang.cabal` => create package definition for numlang library
9) `M323-Haskell/numlang/src/NumLang/NumLang.hs` => move from `M323-Haskell/numlang.hs`, update module header to `NumLang.NumLang`
10) Delete `M323-Haskell/numlang.hs` (moved in step 9)

### Phase 4: Geometry Package
11) Create directory structure: `M323-Haskell/geometry/src/Geometry/` and subdirs `Shapes/TwoD/`, `Shapes/ThreeD/`
12) `M323-Haskell/geometry/geometry.cabal` => create package definition for geometry library
13) `M323-Haskell/geometry/src/Geometry/Vector.hs` => move from `M323-Haskell/vector.hs`, update module header to `Geometry.Vector`
14) Delete `M323-Haskell/vector.hs` (moved in step 13)
15) `M323-Haskell/geometry/src/Geometry/FilterByOrthants.hs` => create placeholder (module header + minimal structure)
16) `M323-Haskell/geometry/src/Geometry/Shapes/TwoD/Circle.hs` => create placeholder
17) `M323-Haskell/geometry/src/Geometry/Shapes/TwoD/Square.hs` => create placeholder
18) `M323-Haskell/geometry/src/Geometry/Shapes/TwoD/Triangle.hs` => create placeholder
19) `M323-Haskell/geometry/src/Geometry/Shapes/ThreeD/Cube.hs` => create placeholder
20) `M323-Haskell/geometry/src/Geometry/Shapes/ThreeD/Sphere.hs` => create placeholder

### Phase 5: Demos Package
21) Create directory structure: `M323-Haskell/demos/app/`, `M323-Haskell/demos/src/`
22) `M323-Haskell/demos/demos.cabal` => create package definition for demos executable
23) `M323-Haskell/demos/app/demo.hs` => move from `M323-Haskell/simple.hs`, convert to Main module with executable entry point
24) Delete `M323-Haskell/simple.hs` (moved in step 23)

### Phase 6: Verification
25) Build verification: run `cabal build all` in M323-Haskell/
26) REPL verification: test `cabal repl geometry`, `cabal repl numlang`, `cabal repl utility`, `cabal repl demos`
27) Functional verification: Load modules and test basic functions

28) **Final @codex Sweep**: scan all touched/new files plus Control Paths (`.vscode/**`, `*.code-workspace`, `M323-Haskell/*.md`, `.editorconfig`) for `@codex` comments; append items to **Developer Interactions**; resolve until none remain.

## Developer Interactions
*(start empty; populate during implementation)*

## Checks & Pass Criteria

### Build Checks
- `cd M323-Haskell; cabal build all` => succeeds without errors
- `cd M323-Haskell; cabal build geometry` => succeeds
- `cd M323-Haskell; cabal build numlang` => succeeds  
- `cd M323-Haskell; cabal build utility` => succeeds
- `cd M323-Haskell; cabal build demos` => succeeds

### REPL Checks
- `cd M323-Haskell; cabal repl geometry` => loads successfully
- `cd M323-Haskell; cabal repl numlang` => loads successfully
- `cd M323-Haskell; cabal repl utility` => loads successfully
- `cd M323-Haskell; cabal repl demos` => loads successfully

### Manual Verification
- [ ] In `cabal repl numlang`: Load `NumLang.NumLang`, test `numlang 42 "de"` => outputs "zweiundvierzig"
- [ ] In `cabal repl geometry`: Load `Geometry.Vector`, test `vadd (V2 1 2) (V2 3 4)` => outputs `V2 4.0 6.0`
- [ ] In `cabal repl utility`: Load `Utility.GHCiPrint`, verify `iprint` function exists
- [ ] In `cabal repl demos`: Load demo module, verify functions from `Simple` are available
- [ ] Verify all placeholder files exist with correct module headers
- [ ] Verify original .hs file content unchanged (only module headers/imports modified)

## Risks / Rollback

### Risk 1: Cabal build failures
- **Impact**: Cannot proceed with restructuring
- **Mitigation**: Follow standard Cabal package structure, validate .cabal files
- **Rollback**: `git revert <commit-sha>` of all restructuring commits

### Risk 2: Module import resolution errors
- **Impact**: Code won't compile/load
- **Mitigation**: Carefully update module headers and ensure all imports are correct
- **Rollback**: `git revert <commit-sha>`

### Risk 3: Lost or corrupted code during file moves
- **Impact**: Functionality broken
- **Mitigation**: Verify file contents before/after each move operation
- **Rollback**: `git checkout HEAD -- <original-file>` before deletion

### Risk 4: REPL functionality broken
- **Impact**: Cannot interactively test code
- **Mitigation**: Test each package REPL individually, update .ghci appropriately
- **Rollback**: `git revert <commit-sha>`, restore original .ghci
