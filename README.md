# M323-Haskell – Multi-Package Cabal Workspace

Funktionale Programmierung mit Haskell: Vektormathematik, Zahlen-zu-Worten Konvertierung, und mehr.

## Projektstruktur

Dieses Projekt ist als **Multi-Package Cabal Workspace** organisiert mit 4 unabhängigen Paketen:

- **`geometry/`** – Vektormathematik und geometrische Formen
  - `Geometry.Vector` – 2D-Vektoren mit Operationen (vadd, vsub, vlength, vunit, vmult, vrotate)
  - `Geometry.Shapes.*` – Platzhalter für zukünftige Form-Implementierungen
- **`numlang/`** – Multi-Sprachen Zahlen-zu-Worten Konvertierung
  - `NumLang.NumLang` – Typeclass-basiert, unterstützt 8 Sprachen (de, en, es, pt, fr, it, tr, jp2)
  - Exakte Rational-Arithmetik mit Periodenerkennung
- **`utility/`** – Gemeinsame Hilfsfunktionen
  - `Utility.GHCiPrint` – Custom REPL-Printer für un-escaped String-Ausgaben
- **`demos/`** – Demo-Executable mit Beispielfunktionen
  - Einfache Übungsfunktionen (third, rectangleArea, num2word, etc.)

## 0a) Installation (einmalig)

```powershell
cd M323-Haskell   # oder: cd M323-Haskell-COPY
````

GHCup via PowerShell (ohne Admin) installieren:
```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;
try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

Danach VS Code neu starten, Tools prüfen:
```powershell
ghcup tui    # empfohlene GHC, cabal, HLS installieren
ghc --version
cabal --version
ghci --version
```

VS Code-Extension: `haskell.haskell`.

## 0b) .ghci (automatisches Setup)

Die `.ghci` Datei im Projektordner wird automatisch geladen und konfiguriert:

* Importiert automatisch `Geometry.Vector`, `NumLang.NumLang`, `Utility.GHCiPrint`
* Aktiviert custom Printer für un-escaped String-Ausgaben
* Unterdrückt Type-Defaulting-Warnungen für saubere REPL-Ausgabe
* Definiert `:r` Reload-Macro zum Wiederherstellen aller Settings

**Du brauchst nichts zu tun** – die Wrapper-Skripte laden `.ghci` automatisch!

## 1) REPL-Workflow (einfach & komfortabel)

### Quick Start

```powershell
# Alle Pakete laden (Windows)
.\ghci.ps1

# Einzelnes Paket (Windows)
.\ghci.ps1 geometry

# Liste erkannter Pakete
.\ghci.ps1 --list
```

```bash
# Unix/Linux/macOS analog
./ghci
./ghci numlang
./ghci --list
```

**GHCi-Optionen:**  
Für GHCi-spezifische Optionen (z.B. `-ignore-dot-ghci`, `-Wall`) nutze die `.ghci`-Datei im Projektordner oder setze sie direkt im REPL mit `:set`.

### Option 1: Wrapper-Skripte (empfohlen)

**PowerShell (Windows):**
```powershell
# Alle Bibliotheken laden (geometry, numlang, utility)
.\ghci.ps1

# Spezifisches Paket laden
.\ghci.ps1 geometry
.\ghci.ps1 numlang
.\ghci.ps1 utility
```

**Bash (Unix/Linux/macOS):**
```bash
# Alle Bibliotheken laden
./ghci

# Spezifisches Paket laden
./ghci geometry
./ghci numlang
```

**Komfort-Tipp (PowerShell-Profil):**  
Für noch schnelleren Zugriff (nur `ghci` statt `.\ghci.ps1` tippen), füge zu deinem PowerShell-Profil hinzu:
```powershell
function ghci {
  param([Parameter(ValueFromRemainingArguments=$true)][string[]]$Args)
  $local = Join-Path $PWD 'ghci.ps1'
  if (Test-Path $local) {
    & $local @Args
  } else {
    # Fallback zu System-GHCi oder cabal repl
    & (Get-Command -Name ghci -CommandType Application | Select-Object -First 1).Source @Args
  }
}
```
_(Profil öffnen: `notepad $PROFILE` – ggf. zuerst `New-Item -Path $PROFILE -ItemType File -Force`)_

### Option 2: Direkt mit Cabal

```powershell
# Alle Bibliotheken (via demos executable)
cabal repl demos:exe:demo

# Einzelne Pakete
cabal repl geometry
cabal repl numlang
cabal repl utility
```

### Im REPL

Alle Module sind bereits geladen – keine manuellen Imports nötig!

```haskell
-- Geometry: Vektoroperationen (direkt verfügbar)
vadd (V2 1 2) (V2 3 4)  -- V2 4.0 6.0
vlength (V2 3 4)        -- 5.0

-- NumLang: Zahlen sprechen (direkt verfügbar)
numlang 42 "de"         -- zweiundvierzig
numlang 69 "fr"         -- soixante-neuf
numlang "328/7" "en"    -- forty-six point repeating eight five seven one four two
numlang (1 % 70) "de"   -- null Komma null Periode eins vier zwei acht fünf sieben

-- Custom Printer ist bereits aktiv (Strings ohne Anführungszeichen)
```

## 2) NumLang – Kurzreferenz

* **Modul:** `NumLang.NumLang`
* **Export:** `numlang`
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

* **Wrapper-Skript wird nicht erkannt (PowerShell)**
  - Ausführungsrichtlinie: `Set-ExecutionPolicy -Scope CurrentUser RemoteSigned`
  - Explizit ausführen: `powershell -ExecutionPolicy Bypass -File .\ghci.ps1`

* **Module nicht gefunden**
  - Stelle sicher, dass du im M323-Haskell-Verzeichnis bist
  - Einmal `cabal build all` ausführen

* **VS Code findet HLS/GHC nicht**
  - `C:\ghcup\bin` muss in PATH sein
  - VS Code neu starten nach GHCup-Installation

## 4) Bauen & Testen

```powershell
# Alle Pakete bauen
cabal build all

# Einzelne Pakete bauen
cabal build geometry
cabal build numlang

# Demo-Programm ausführen
cabal run demos:exe:demo
```

## 5) Projekt erweitern

Neues Paket hinzufügen:
1. Erstelle Verzeichnis mit `<package>.cabal` Datei
2. Füge Package zu `cabal.project` hinzu
3. Die Wrapper-Skripte erkennen es automatisch!
