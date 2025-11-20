# M323-Haskell – Cabal Workspace & PowerShell Helper 

Modular Haskell workspace mit einem PowerShell-Skript (`hs.ps1`) für schnellen, bequemen REPL-Zugriff auf mehrere Libraries gleichzeitig.

---

## TL;DR – Wie starte ich den REPL?

1. **Im Projektroot arbeiten**

```powershell
cd M323-Haskell
```

2. **Setup-Skript ansteuern**

Du musst dir den Dateinamen nicht merken – tippe einfach:
```powershell
hs<TAB>
```
und PowerShell vervollständigt zu:
```powershell
.\hs.ps1
```

3. **Packages auswählen (Argument-Varianten)**

Für jeden Package-Namen kannst du **jede** der folgenden Formen nutzen:

```powershell
.\hs.ps1 numlang geometry
.\hs.ps1 .\numlang\ .\geometry\
.\hs.ps1 ./numlang/ ./geometry/
```

Alle drei Varianten landen intern wieder bei `numlang` bzw. `geometry` – das Skript normalisiert solche Pfade automatisch.

4. **Hilfe & Package-Liste**

```powershell
# nur die in cabal.project registrierten Packages anzeigen
.\hs.ps1 --list
.\hs.ps1 -list
.\hs.ps1 list

# Hilfe + Beispiele + Package-Liste
.\hs.ps1 --help
.\hs.ps1 -help
.\hs.ps1 help
```

**Hinweis**: Diese Modi funktionieren nur, wenn **genau ein Argument** übergeben wird.

---

## Workspace Structure

Dies ist ein **Cabal Multi-Package Workspace** mit:
* **`geometry/`** – Library für 2D-Vektor-Operationen (`Geometry.Vector`)
* **`numlang/`** – Library für Multi-Language Number-to-Words (`NumLang.NumLang`)
* **`utility/`** – Library mit Custom REPL Printer (`Utility.GHCiPrint`)
* **`demos/`** – Executable, das alle Libraries zusammenführt
* **`startup/`** – interne Library, deren Modul `Startup.Startup` automatisch generiert wird
* **`cabal.project`** – Workspace-Konfiguration (inkl. `packages:`-Liste)

### Module Hierarchy

```text
Geometry.Vector     -- 2D vectors, type-safe operations (GADTs, TypeFamilies)
NumLang.NumLang     -- numlang function (typeclass-based, supports Integer/Rational/String)
Utility.GHCiPrint   -- iprint: raw string/text output for REPL
Startup.Startup     -- automatisch generiertes Sammelmodul für REPL
Main                -- demo executable (simple.hs examples)
```

---

## Prerequisites

* GHC (9.0+) und Cabal (3.0+) via [GHCup](https://www.haskell.org/ghcup/)
  * (nicht vergessen, anschliessend VSCode neuzustarten)
* Optional: VS Code + `haskell.haskell`-Extension

---

## Das `hs.ps1`-Skript im Detail

### Was das Skript macht

Beim Aufruf aus dem Projektroot:

```powershell
.\hs.ps1 [PACKAGE...]
```

passiert intern:

1. **`cabal.project` wird gelesen**

   * Alle Einträge unter `packages:` (z.B. `geometry/`, `numlang/`, …) werden extrahiert und zu nackten Paketnamen normalisiert (`geometry`, `numlang`, `utility`, `demos`, `startup`).

2. **Argumente werden normalisiert**

   * `numlang`, `./numlang/` und `.\numlang\` werden alle als `numlang` interpretiert.

3. **Spezialfälle werden behandelt**

   * `--list` / `-list` / `list` → nur Paket-Liste ausgeben, dann Exit.
   * `--help` / `-help` / `help` → Paket-Liste + Beispiel-Aufrufe ausgeben, dann Exit.

4. **Unbekannte Pakete werden ignoriert**

   * Wenn ein Argument nicht in `cabal.project` vorkommt → Warnung, dieses Paket wird übersprungen.

5. **Zu jedem Paket wird `<pkg>.cabal` gefunden**

   * Bevorzugt `<root>\<pkg>\<pkg>.cabal`, sonst rekursiv gesucht, `dist-newstyle` wird ignoriert.

6. **Aus jeder `.cabal` werden die `exposed-modules` der `library` gelesen**

7. **Alle Module werden aggregiert & dedupliziert**

   * Reihenfolge entspricht der Reihenfolge deiner angegebenen Packages.

8. **`startup/src/Startup/Startup.hs` wird generiert**

Das Skript erzeugt ein Modul der Form:
```haskell
module Startup.Startup
  ( module NumLang.NumLang
  , module Geometry.Vector
  , module Utility.GHCiPrint
  ) where

import NumLang.NumLang
import Geometry.Vector
import Utility.GHCiPrint
```

(Die tatsächliche Liste hängt von deinen gewählten Packages ab.)

9. **`.ghci` wird sichergestellt / erweitert**

   * Falls keine `.ghci` im Projektroot existiert → sie wird mit
     `import Startup.Startup` angelegt.
   * Falls `.ghci` existiert, aber diese Zeile fehlt → sie wird ergänzt.

10. **`cabal repl startup` wird gestartet**

    * Du landest direkt im GHCi mit allen Modulen im Scope (via `Startup.Startup`).

---

## Typische Aufrufe

Aus `cd M323-Haskell`:
```powershell
# alle Packages aus cabal.project (außer 'startup')
.\hs.ps1

# nur numlang
.\hs.ps1 numlang
.\hs.ps1 .\numlang\
.\hs.ps1 ./numlang/

# geometry + numlang
.\hs.ps1 geometry numlang

# 3 Packages mit verschiedenen validen Pfad-Formen
.\hs.ps1 ./geometry/ numlang .\utility\

# nur die verfügbaren Packages anzeigen
.\hs.ps1 --list

# Hilfe + Beispiele
.\hs.ps1 --help
```

Wenn du magst, kannst du dir zusätzlich eine Abkürzung `hs` einrichten (optional):

```powershell
notepad $PROFILE

# hinzufügen:
function hs {
  param([Parameter(ValueFromRemainingArguments=$true)][string[]]$Args)
  $local = Join-Path $PWD 'hs.ps1'
  if (Test-Path $local) { & $local @Args } else { cabal repl @Args }
}
```

Danach reicht in diesem Repo:

```powershell
hs geometry numlang
hs --list
hs
```

---

## REPL Workflow

Mit einem einfachen:

```powershell
.\hs.ps1
```

startest du `cabal repl startup`, `Startup.Startup` wird importiert und stellt alle gewünschten Module zur Verfügung.

Beispiele im REPL (analog zur bisherigen Nutzung):
```haskell
ghci> :browse Geometry.Vector
V2 :: Double -> Double -> V2
vadd :: V2 -> V2 -> V2
vsub :: V2 -> V2 -> V2
...

ghci> :browse NumLang.NumLang
numlang :: NumLang a => a -> String -> String

ghci> numlang 69 "fr"
"soixante-neuf"

ghci> numlang "328/7" "de"
"sechsundvierzig Komma Periode acht fuenf sieben eins vier zwei"

ghci> vadd (V2 1 2) (V2 3 4)
V2 4.0 6.0
```

Reload nach Code-Änderungen:

```haskell
ghci> :r
```

Beenden:

```haskell
ghci> :q
```

---

## 2) numlang – Kurzreferenz

* **Export:** `NumLang.numlang` (im REPL dank `Startup.Startup` indirekt im Scope).
* **Akzeptierte Eingaben:**

  * `Integer` / `"Integer"` / `"Integer" mit Tausendertrennzeichen`
  * `"a/b"` (Bruch, exakt)
  * `"n.m"` (Dezimalstring; Fractional-Teil wird **hart** auf `3 * limitDecimals` gekürzt, keine Rundung)
  * Ausdrücke mit grossen Ergebnissen in GHCi als `Integer` (z. B. `(10^1234 + 42*69)`), solange das Ergebnis `Integer` ist.
* **Double/Float:**
  Wenn `show x` Exponent anzeigt oder > ~15 Ziffern → Hinweis: „Bitte als String senden …“.
  Für periodische Dezimaldarstellung immer `Rational`/String verwenden.
* **Dezimal-Ausgabe:**
  * Periode wird exakt erkannt (Rest-Tabelle) und nur **einmal** genannt.
  * Nichtperiodischer `pre`-Teil wird auf `limitDecimals` (derzeit 50) gekürzt (Anzeige, nicht Erkennung - diese liegt bei `3*limitDecimals` (derzeit 150)).

Beispiele:

```haskell
numlang "1/47" "de"
numlang "1/7"  "en"

-- Rational direkt:
numlang (1 % 70) "de"
```

---

## 3) Troubleshooting

* **Escaped Strings / `\n` sichtbar?**
  Dann ist `Utility.GHCiPrint.iprint` wahrscheinlich nicht aktiv. `.ghci` sollte `-interactive-print=GHCiPrint.iprint` setzen.
* **„Variable not in scope: numlang“**
  Kontext verloren (z.B. manuelles `:load`). Lösung: `:r` oder `import NumLang.NumLang` / `import Startup.Startup` neu.
* **„module 'main:Main' is defined in multiple files“**
  Nur ein `Main` gleichzeitig. Unsere Beispiel-Files sind benannte Module, das Problem tritt nur bei eigenen `Main`-Modulen auf.
* **VS Code findet HLS/GHC nicht**
  `C:\ghcup\bin` in `PATH` und VS Code neu starten.

---

## 4) Optional: Cabal-Befehle

Standard-Cabal-Workflow bleibt natürlich möglich: 

```bash
cabal build
cabal run
cabal repl
```
