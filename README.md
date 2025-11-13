# M323-Haskell – Cabal Workspace

Modular Haskell workspace with auto-discovery wrappers for quick REPL access.

## Workspace Structure

This is a **Cabal multi-package workspace** with:

- **`geometry/`** – Library for 2D vector operations (`Geometry.Vector`)
- **`numlang/`** – Library for multi-language number-to-words conversion (`NumLang.NumLang`)
- **`utility/`** – Library with custom REPL printer (`Utility.GHCiPrint`)
- **`demos/`** – Executable aggregator that depends on all libraries
- **`cabal.project`** – Workspace configuration (multi-repl enabled)

### Module Hierarchy
```
Geometry.Vector     -- 2D vectors, type-safe operations (GADTs, TypeFamilies)
NumLang.NumLang     -- numlang function (typeclass-based, supports Integer/Rational/String)
Utility.GHCiPrint   -- iprint: raw string/text output for REPL
Main                -- demo executable (simple.hs examples)
```

## Prerequisites

- GHC (9.0+) and Cabal (3.0+) installed via [GHCup](https://www.haskell.org/ghcup/)
- VS Code with `haskell.haskell` extension (optional, for IDE features)

## Quick Start

### 1) Build the workspace

```powershell
cd M323-Haskell
cabal build all
```

### 2) REPL Access via Wrappers

**PowerShell wrapper** (`hs.ps1`) provides auto-discovery and smart loading:

```powershell
# List available packages
.\hs.ps1 --list

# Load demos (all libraries available)
.\hs.ps1

# Load specific library + utility
.\hs.ps1 geometry
.\hs.ps1 numlang

# Pass additional args to cabal repl
.\hs.ps1 -- --repl-options -v
```

**Optional: Add to PowerShell profile** for `hs` command anywhere:
```powershell
notepad $PROFILE
# Add:
function hs { param([Parameter(ValueFromRemainingArguments=$true)][string[]]$Args)
  $local = Join-Path $PWD 'hs.ps1'
  if (Test-Path $local) { & $local @Args } else { cabal repl @Args }
}
```

### 3) REPL Workflow

```powershell
.\hs.ps1
```

In the REPL (all modules loaded, custom printer active):

```haskell
ghci> :browse Geometry.Vector
V2 :: Double -> Double -> V2
vadd :: V2 -> V2 -> V2
vsub :: V2 -> V2 -> V2
...

ghci> :browse NumLang.NumLang
numlang :: NumLang a => a -> String -> String

ghci> numlang (69 :: Integer) "fr"
soixante-neuf

ghci> numlang "328/7" "de"
sechsundvierzig Komma acht Periode fünf sieben eins vier zwei

ghci> vadd (V2 1 2) (V2 3 4)
V2 4.0 6.0
```

**Reload after changes:**
```haskell
ghci> :r
```

**Exit:**
```haskell
ghci> :q
```

## 2) numlang – Kurzreferenz

* **Exports:** `NumLang.numlang` (im REPL dank `.ghci` unqualifiziert verfügbar).
* **Akzeptierte Eingaben:**
  * `Integer` / `"Integer"` / `"Integer" mit Tausendertrennzeichen`
  * `"a/b"` (Bruch, exakt)
  * `"n.m"` (Dezimalstring; Fractional-Teil wird **hart** auf `3 * limitDecimals` gekürzt, keine Rundung)
  * Ausdrücke in **GHCi** als Integer (z. B. `(10^1234 + 42*69)`), sofern Ergebnis `Integer` ist
* **Double/Float:** Wenn `show x` Exponent zeigt oder >15 (Gesamt-/Dezimal-)Ziffern ⇒ Hinweis: „Bitte als String senden …“. (Für Perioden **immer** `Rational`/String nutzen.)
* **Dezimal-Ausgabe:**
  * Periode wird **exakt** erkannt (Rest-Tabelle) und **nur einmal** genannt.
  * Nichtperiodischer `pre`-Teil wird auf `limitDecimals` (derzeit 50) gekürzt (Anzeige – nicht Erkennung).

Beispiele:
```ghci
numlang "1/47" "de"
numlang "1/7"  "en"
-- Rational direkt:
numlang (1 % 70) "de"
```

## 3) Troubleshooting

* **Escaped Strings / `\n` sichtbar?**
  Das passiert nur, wenn der Printer nicht aktiv ist. `.ghci` setzt `-interactive-print=GHCiPrint.iprint`. Falls du manuell andere Files lädst, nutze `:r` (Macro setzt Printer neu). `iprint` zeigt Strings/Text/ByteString **roh**.

* **„Variable not in scope: numlang“**
  Kontext verloren (z.B. durch manuelles `:load`). Entweder `:r` benutzen **oder** `:module + NumLang Simple` erneut setzen. `.ghci` tut das automatisch beim Start und beim `:r`-Macro.

* **„module `main:Main` is defined in multiple files“**
  Nur **ein** `Main` gleichzeitig. Unsere Dateien sind benannte Module (`module NumLang …`, `module Simple …`) – so gibt’s keinen Konflikt.

* **VS Code findet HLS/GHC nicht**
  `C:\ghcup\bin` gehört in PATH; danach VS Code neu starten.

## 4) Optional: Cabal

```bash
cabal init --libandexe -n --source-dir=src --application-dir=app --language=GHC2021
cabal build
cabal run
cabal repl
```
