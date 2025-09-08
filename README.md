# M323-Haskell – numlang & more

Ziel: schnell ins REPL, Module laden, Zahlen „sprechen“ (DE/EN), und schöne, **un-escaped** Ausgaben sehen.

## Projektstruktur

- `GHCiPrint.hs` – Custom-Printer `iprint` für REPL: Strings/Text/ByteString **roh** (echte Newlines, keine Quotes).
- `NumLang.hs` – Hauptmodul mit `numlang` (Typeclass + Instanzen für `Integer`, `Rational`, `Double`, `Float`, `String`), Parser und periodischer Dezimal-Erkennung. Export: `numlang`.
- `Simple.hs` – kleine Beispiel-/Übungsfunktionen (z.B. `num2word`, wovon numlang inspiriert ist).
- `.ghci` – Session-Konfiguration (lädt Module, setzt Printer, richtet `:r`-Macro ein).

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

Lege im Projektordner `.ghci` an (falls noch nicht vorhanden) mit:

```ghci
:load GHCiPrint NumLang Simple
:module + NumLang Simple
:set -interactive-print=GHCiPrint.iprint
:def! r \_ -> return ":reload\n:module + NumLang Simple\n:set -interactive-print=GHCiPrint.iprint"
```

* Lädt Printer & Code-Module.
* Bringt `NumLang` & `Simple` **in den Prompt-Scope**, damit Funktionen **ohne** Qualifier verfügbar sind.
* Stellt sicher, dass `:r` (Reload) **immer** Kontext & Printer neu setzt.

## 1) REPL-Workflow (täglich)

```powershell
ghci
```

Jetzt sind `numlang` & Co. direkt da (unescaped Output):

```ghci
-- Integer
numlang "32847218746876234" "de"

-- Bruch (String → exakt Rational → Periode)
numlang "328/7" "de"
numlang "328/7" "en"

-- Beispiele aus Simple
num2word 3
```

**Reload nach Änderungen:**
Einfach `:r` – der `.ghci`-Macro kümmert sich um alles.

**Neue Datei hinzufügen:**

* Schnell testen: `:add NeuesModul(.hs)` (fügt hinzu, ohne anderes zu entladen)
* Dauerhaft: `.ghci` um die Datei erweitern und `:r`

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
