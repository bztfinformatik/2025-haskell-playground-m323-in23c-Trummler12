# hs Wrapper - Ablaufanalyse (Step-by-Step)

Dieses Dokument erklärt **genau**, was passiert, wenn du `hs` oder `hs numlang geometry` eingibst.

---

## Szenario 1: `hs` (keine Argumente)

### Schritt 1: Auto-Discovery (Zeilen 4-24)
**IN:** Dateisystem unter `.` (rekursiv, max. Tiefe 2)  
**OPERATION:**
```powershell
Get-ChildItem -Recurse -Depth 2 -Filter *.cabal | 
  Where-Object { $_.FullName -notmatch '\\dist-newstyle\\' }
```
**OUT:**
- `$allPkgs = ["demos", "geometry", "numlang", "utility"]`
- `$libPkgs = ["geometry", "numlang", "utility"]` (nur die mit `library` Stanza)

**Zweck:** Wir wissen jetzt, welche Pakete im Workspace existieren.

---

### Schritt 2: Argument-Normalisierung (Zeilen 65-68)
**IN:** `$Args = @()` (leeres Array, da keine Argumente)  
**OPERATION:**
```powershell
$normalized = @()
foreach ($a in $Args) {
  $normalized += (Normalize-Target $a)
}
```
**OUT:** `$normalized = @()` (bleibt leer)

---

### Schritt 3: Pfad-Check (Zeile 63)
**IN:** Dateisystem  
**OPERATION:**
```powershell
$hasDemos = Test-Path ".\demos\demos.cabal"
```
**OUT:** `$hasDemos = $true`

---

### Schritt 4: Routing-Entscheidung (Zeile 72)
**IN:** `$normalized.Count = 0`  
**OPERATION:** Bedingung `if ($normalized.Count -eq 0)` ist **wahr**  
**OUT:** Wir gehen in den **Aggregator-Modus** (Zeilen 73-84)

---

### Schritt 5: .ghci-Skript auswählen (Zeile 74)
**IN:** Leerer String `''` (kein spezifisches Paket)  
**OPERATION:**
```powershell
function GhciScript-For([string]$pkg) {
  if ([string]::IsNullOrWhiteSpace($pkg)) {
    $p = "./.ghci-all"  # <-- Dieser Pfad wird gewählt
  } else {
    $p = "./.ghci"
  }
  if (Test-Path $p) {
    return (Resolve-Path $p).Path
  } else {
    return $rootGhci
  }
}
```
**OUT:** `$script = "C:\...\M323-Haskell\.ghci-all"` (absoluter Pfad)

**Inhalt von `.ghci-all`:**
```haskell
:m + Geometry.Vector NumLang.NumLang Utility.GHCiPrint
:set -interactive-print=Utility.GHCiPrint.iprint
```

---

### Schritt 6: demos:exe:demo existiert → Cabal-Befehl (Zeilen 75-76)
**IN:**
- `$hasDemos = $true`
- `$script = "C:\...\M323-Haskell\.ghci-all"`

**OPERATION:**
```powershell
cabal repl 'demos:exe:demo' --repl-options "-ghci-script=$script"
```

**Expandiert zu:**
```bash
cabal repl 'demos:exe:demo' --repl-options "-ghci-script=C:\Users\tobia\Documents\GitHub\ProgrammierZeugs\M323-Haskell\.ghci-all"
```

**OUT (finale Cabal-Invokation):**
```
Cabal lädt:
1. Komponente: demos:exe:demo (Executable mit Abhängigkeiten zu geometry, numlang, utility)
2. GHCi startet mit --ghci-script Flag → lädt .ghci-all
3. .ghci-all führt aus:
   - :m + Geometry.Vector NumLang.NumLang Utility.GHCiPrint
   - :set -interactive-print=Utility.GHCiPrint.iprint
4. Alle Module sind im Scope: vadd, V2, numlang, iprint
```

---

### Zusammenfassung Szenario 1: `hs`

| Phase | Input | Output |
|-------|-------|--------|
| 1. Discovery | Dateisystem | `allPkgs=4, libPkgs=3` |
| 2. Normalisierung | `$Args=[]` | `$normalized=[]` |
| 3. demos-Check | Dateisystem | `$hasDemos=true` |
| 4. Routing | `$normalized.Count=0` | Aggregator-Modus |
| 5. .ghci-Auswahl | `pkg=''` | `.ghci-all` (absoluter Pfad) |
| 6. Cabal-Aufruf | `demos:exe:demo` + `.ghci-all` | **`cabal repl 'demos:exe:demo' --repl-options "-ghci-script=C:\...\\.ghci-all"`** |

**Finaler Effekt:** GHCi lädt `demos:exe:demo` (mit allen Lib-Dependencies) + `.ghci-all` importiert alle Module → voller Zugriff auf Geometry, NumLang, Utility.

---

## Szenario 2: `hs numlang geometry`

### Schritt 1: Auto-Discovery (identisch zu Szenario 1)
**OUT:**
- `$allPkgs = ["demos", "geometry", "numlang", "utility"]`
- `$libPkgs = ["geometry", "numlang", "utility"]`

---

### Schritt 2: Argument-Normalisierung (Zeilen 65-68)
**IN:** `$Args = @("numlang", "geometry")`  
**OPERATION:**
```powershell
$normalized = @()
foreach ($a in $Args) {
  $normalized += (Normalize-Target $a)
}

# Normalize-Target Funktion:
function Normalize-Target([string]$t) {
  $t = $t.Trim()
  $t = $t -replace '^[.\\/]+', ''   # entfernt .\, ./, .//, etc.
  $t = $t -replace '[\\/]+$', ''    # entfernt trailing \ oder /
  return $t
}
```
**OUT:**
- `Normalize-Target("numlang")` → `"numlang"`
- `Normalize-Target("geometry")` → `"geometry"`
- `$normalized = @("numlang", "geometry")`

---

### Schritt 3: Routing-Entscheidung (Zeile 86)
**IN:**
- `$normalized.Count = 2` (≠ 0)
- `$normalized[0] = "numlang"` (∈ `$allPkgs`?)
- Bedingung `$normalized.Count -eq 1` ist **falsch**

**OPERATION:** Wir springen zu `else`-Block (Zeilen 92-96)

---

### Schritt 4: Multi-REPL Warnung (Zeilen 93-95)
**IN:** Multi-Target erkannt  
**OPERATION:**
```powershell
Write-Host "Hinweis: Mehrere Targets starten die experimentelle Cabal-Multi-REPL." -ForegroundColor Yellow
Write-Host "Auto-Imports aus .ghci-all sind hier absichtlich deaktiviert." -ForegroundColor Yellow
Write-Host "Nutzen Sie 'import Geometry.Vector' bzw. 'import NumLang.NumLang' manuell." -ForegroundColor Yellow
```
**OUT:** Gelbe Konsolen-Ausgabe (User-Info)

---

### Schritt 5: .ghci-Skript (neutrales Root-Skript)
**IN:** Multi-REPL Modus  
**OPERATION:**
```powershell
$rootGhci = (Resolve-Path "./.ghci").Path
```
**OUT:** `$rootGhci = "C:\...\M323-Haskell\.ghci"` (absoluter Pfad)

**Inhalt von `.ghci` (neutral):**
```haskell
-- .ghci (Root): nur globale Flags, keine Imports
:set -Wall
:set -fno-warn-type-defaults
:set prompt "ghci> "
```

---

### Schritt 6: Cabal-Befehl (Zeile 96)
**IN:**
- `$Args = @("numlang", "geometry")` (Original-Argumente, **nicht** normalisiert!)
- `$rootGhci = "C:\...\M323-Haskell\.ghci"`

**OPERATION:**
```powershell
cabal repl @Args --repl-options "-ghci-script=$rootGhci"
```

**PowerShell Splatting (`@Args`):** Expandiert Array zu einzelnen Argumenten  
**Expandiert zu:**
```bash
cabal repl numlang geometry --repl-options "-ghci-script=C:\Users\tobia\Documents\GitHub\ProgrammierZeugs\M323-Haskell\.ghci"
```

**OUT (finale Cabal-Invokation):**
```
Cabal lädt:
1. Multi-REPL für: numlang (lib:numlang) + geometry (lib:geometry)
2. GHCi startet mit --ghci-script Flag → lädt .ghci (neutral)
3. .ghci führt aus (nur Flags):
   - :set -Wall
   - :set -fno-warn-type-defaults
   - :set prompt "ghci> "
4. KEINE Auto-Imports! Module müssen manuell importiert werden:
   ghci> import Geometry.Vector
   ghci> import NumLang.NumLang
```

---

### Zusammenfassung Szenario 2: `hs numlang geometry`

| Phase | Input | Output |
|-------|-------|--------|
| 1. Discovery | Dateisystem | `allPkgs=4, libPkgs=3` |
| 2. Normalisierung | `["numlang", "geometry"]` | `["numlang", "geometry"]` (keine Änderung) |
| 3. Routing | `Count=2` | Multi-REPL Modus |
| 4. Warnung | - | Gelbe Konsolen-Ausgabe |
| 5. .ghci-Auswahl | Multi-Modus | `.ghci` (neutral, absoluter Pfad) |
| 6. Cabal-Aufruf | `numlang geometry` + `.ghci` | **`cabal repl numlang geometry --repl-options "-ghci-script=C:\...\\.ghci"`** |

**Finaler Effekt:** GHCi startet experimentelle Multi-REPL mit `numlang` und `geometry` (beide Libraries geladen, aber Module **nicht** automatisch im Scope). Nur globale GHCi-Flags aus `.ghci` aktiv. User muss `import ...` manuell eingeben.

---

## Wichtige Erkenntnisse für Vereinfachung

### 1. Cabal-Targets (was Cabal versteht)

Cabal akzeptiert diese Target-Formate:
- `demos:exe:demo` - Vollqualifiziert (Paket:Komponente:Name)
- `lib:numlang` - Kurzform für Library (Paket:Komponente)
- `numlang` - Bare Name (Cabal sucht automatisch die Library)

**Aktuell:** Wrapper nutzt `lib:numlang` explizit (Zeile 89).  
**Vereinfachung möglich?** `numlang` würde auch funktionieren (Cabal inferiert `lib:numlang`), spart 4 Zeichen. Aber: explizit ist klarer bei Fehlermeldungen.

---

### 2. .ghci-Skript Mechanismus

**`--repl-options "-ghci-script=/absolute/path"`:**
- Cabal startet GHCi
- GHCi führt das Skript **nach** dem Laden der Komponenten aus
- Timing-Problem bei Single-Lib: Wenn `.ghci-numlang` `:m + NumLang.NumLang` enthält, ist das Paket noch "hidden" → Fehler
- Lösung: Leere `.ghci-<pkg>` Skripte, Cabal setzt Kontext automatisch

**Vereinfachung möglich?**
- `.ghci-numlang` / `.ghci-geometry` könnten **komplett gelöscht** werden (aktuell nur Prompt-Anpassung)
- `.ghci` (neutral) reicht für alle Single-Lib Cases
- Nur `.ghci-all` ist wirklich nötig (für Aggregator)

---

### 3. Normalize-Target Funktion

**Zweck:** Tab-Completion in PowerShell erzeugt `.\numlang\` → muss zu `numlang` werden.

**Regex-Operationen:**
```powershell
$t -replace '^[.\\/]+', ''   # entfernt ., .., ./, .\\, .//, etc. am Anfang
$t -replace '[\\/]+$', ''    # entfernt /, \, //, etc. am Ende
```

**Testfälle:**
| Input | Output |
|-------|--------|
| `numlang` | `numlang` |
| `.\numlang` | `numlang` |
| `.\numlang\` | `numlang` |
| `./numlang/` | `numlang` |
| `..\numlang` | `..\numlang` (nur führende `.` entfernt, nicht `..`) |

**Vereinfachung möglich?** Nein, diese Funktion ist minimal und notwendig.

---

### 4. Multi-REPL Limitierungen (Cabal)

**Warum "Command is not supported (yet) in multi-mode"?**
- Cabal's Multi-REPL ist **experimentell** (seit Cabal 3.11)
- Manche GHCi-Kommandos (`:set`, `:m`) funktionieren nicht zuverlässig
- `.ghci-all` würde Fehler werfen in Multi-Mode → deshalb neutrales `.ghci`

**Vereinfachung möglich?** Nein, das ist eine Cabal-Einschränkung. Wir können nur:
1. User warnen (✅ bereits implementiert)
2. Neutrales `.ghci` verwenden (✅ bereits implementiert)
3. Dokumentieren, dass manuelle Imports nötig sind (✅ in Warnung)

---

## Potenzielle Vereinfachungen (Bewertung)

### ✅ Empfohlen: .ghci-<pkg> Dateien löschen
**Aktuell:** `.ghci-numlang`, `.ghci-geometry` existieren, sind aber nur Prompt-Anpassungen.  
**Vereinfachung:**
```bash
rm .ghci-numlang .ghci-geometry
```
**Effekt:** Single-Lib Cases nutzen `.ghci` (neutral). Funktioniert identisch, weniger Dateien.

---

### ❌ Nicht empfohlen: `lib:` Präfix weglassen
**Aktuell:** `cabal repl 'lib:numlang'`  
**Alternativ:** `cabal repl numlang`  
**Problem:** Bei Paket-Namen, die auch als Executable existieren, kann Cabal den falschen Target wählen. Explizit ist sicherer.

---

### ✅ Optional: Auto-Discovery cachen
**Aktuell:** Jeder `hs` Aufruf scannt `.cabal` Dateien neu.  
**Vereinfachung:** Cache in `.hs-cache` (JSON), regenerieren nur wenn `.cabal` Dateien sich ändern.  
**Trade-off:**
- ➕ Schnellerer Start (1-5 ms Ersparnis bei 4 Paketen)
- ➖ Komplexität (Cache-Invalidierung bei File-Änderungen)
- ➖ Lohnt sich erst ab ~10+ Paketen

**Empfehlung:** Für dein 4-Paket-Setup **nicht nötig**. Bei größeren Monorepos (20+ Pakete) würde es Sinn machen.

---

### ✅ Empfohlen: Dokumentation inline (Code-Kommentare)
**Aktuell:** Kommentare in `hs.ps1` knapp.  
**Vereinfachung:** ASCII-Diagramm am Anfang:
```powershell
# Routing:
#   hs              -> demos:exe:demo + .ghci-all (alle Module)
#   hs numlang      -> lib:numlang + .ghci (neutral)
#   hs numlang geom -> numlang geometry + .ghci (neutral, Multi-REPL)
```

---

## Finale Befehle (Cheat Sheet)

### Szenario 1: `hs`
```bash
cabal repl 'demos:exe:demo' \
  --repl-options "-ghci-script=C:\...\M323-Haskell\.ghci-all"
```
**Was passiert:**
1. Cabal baut `demos:exe:demo` (hat Dependencies zu geometry, numlang, utility)
2. GHCi startet, lädt Executable
3. `.ghci-all` importiert alle Module: `Geometry.Vector`, `NumLang.NumLang`, `Utility.GHCiPrint`
4. `iprint` wird als custom Printer gesetzt
5. Alle Funktionen (`vadd`, `V2`, `numlang`) sind im Scope

---

### Szenario 2: `hs numlang`
```bash
cabal repl 'lib:numlang' \
  --repl-options "-ghci-script=C:\...\M323-Haskell\.ghci"
```
**Was passiert:**
1. Cabal baut `lib:numlang`
2. GHCi startet, lädt Library
3. `.ghci` setzt nur Flags (`:set -Wall`, Prompt)
4. Cabal setzt Modul-Kontext automatisch auf `*NumLang.NumLang`
5. `numlang` Funktion ist im Scope

---

### Szenario 3: `hs numlang geometry`
```bash
cabal repl numlang geometry \
  --repl-options "-ghci-script=C:\...\M323-Haskell\.ghci"
```
**Was passiert:**
1. Cabal startet experimentelle Multi-REPL
2. Baut `lib:numlang` und `lib:geometry`
3. GHCi startet, lädt beide Libraries
4. `.ghci` setzt nur Flags
5. **Keine** Auto-Imports! User muss `import NumLang.NumLang` und `import Geometry.Vector` manuell eingeben
6. Manche `.ghci` Kommandos funktionieren nicht (Cabal-Limitation)

---

## Fragen für weitere Optimierung

1. **Brauchst du wirklich die Prompt-Anpassungen** in `.ghci-numlang` / `.ghci-geometry`?  
   → Falls nein: Dateien löschen, spart 2 Dateien.

2. **Wie oft nutzt du Multi-REPL** (`hs numlang geometry`)?  
   → Falls selten: Könnte man komplett entfernen, nur Single-Lib + Aggregator unterstützen.

3. **Möchtest du Bash-Wrapper (`hs`) auch analysieren**?  
   → Gleiche Logik wie PowerShell, nur Bash-Syntax.

4. **Cache-Bedarf?**  
   → Nur sinnvoll bei 10+ Paketen. Aktuell: 4 Pakete, Auto-Discovery dauert ~2-5 ms.

---

**Ende der Analyse.** Lass mich wissen, welche Vereinfachungen du umsetzen möchtest!
