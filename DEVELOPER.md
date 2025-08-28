# M323-Haskell – DEVELOPER.md (Windows 11)

Ziel: Schnell startklar für Modul 323 „Funktional programmieren“ mit Haskell.
Scope: Nur Inhalte, die den M323-Lernzielen dienen (Funktionen als Werte, HOFs,
Lambdas, map/filter/reduce, deklaratives Denken, Refactoring & einfache Performance).

---

## 0) Installation & Setup (Beginner / Essenziell)

### 0.1 Toolchain (Windows 11, PowerShell)
* **Haskell Toolchain via GHCup** (offiziell empfohlen):
  In **PowerShell** (ohne Admin) ausführen:

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;
try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```

*GHCup installiert GHC, `cabal`, optional `stack` und Haskell Language Server (HLS) – inkl. MSYS2 Toolchain für Windows.*
Danach in derselben PowerShell:

```powershell
ghcup tui   # interaktives Tool: GHC (recommended), cabal, HLS, optional stack installieren
```

> „recommended“ stellt in der Regel die beste Kompatibilität (z. B. mit HLS) sicher.
> Quellen: GHCup Installationsanleitung & empfohlene Kanäle. [1]

* **VS Code Haskell-Extension**:
In VS Code `Ctrl+P` → `ext install haskell.haskell`. [2]

**Checks**:

```powershell
ghc --version
cabal --version
ghci --version
# optional
stack --version
```

> Hinweise:
>
> * GHCup verwaltet Versionen; auf Windows wird MSYS2 automatisch mit eingerichtet. [1] [3]
> * Falls VS Code HLS nicht findet, sicherstellen, dass `C:\ghcup\bin` in PATH ist und VS Code neu starten. [1] [4]

---

## 1) Erste Schritte mit Compiler & REPL (Beginner / Essenziell)

* **REPL starten**:

```powershell
ghci
```

Nützliche REPL‑Kommandos:

* `:load Datei.hs` / `:reload` – Modul laden / neu laden
* `:type expr` oder `:t expr` – Typ anzeigen
* `:info name` oder `:i name` – Typ-/Klassendetails
* `:quit` – beenden
  (Siehe GHC User’s Guide: GHCi commands.) [5]

* **Einzeldatei kompilieren**:

```powershell
ghc --make Main.hs -o main.exe
.\main.exe
```

---

## 2) Projekt aufsetzen (Cabal – empfohlen fürs Modul)

### 2.1 Neues Projekt (Executable + Library)

```powershell
cabal init --libandexe -n ^
--source-dir=src ^
--application-dir=app ^
--language=GHC2021
```

* Dadurch entstehen u. a.:

```
m323-haskell.cabal   # Projektdefinition
app/Main.hs          # Einstieg (I/O, klein halten)
src/Lib.hs           # Reine Logik/Funktionen – HOFs, map/filter/fold
```

(Die `--libandexe`‑Option erzeugt Library **und** Executable in der üblichen Struktur.) [6]

**Build/Run/REPL**:

```powershell
cabal build
cabal run
cabal repl        # REPL im Projektkontext
```

### 2.2 (Optional) Projekt mit Stack

```powershell
# Stack ggf. per GHCup installiert; alternativ:
winget install -e --id=CommercialStack.Stack
# Projekt anlegen:
stack new m323-haskell simple
cd m323-haskell
stack build
stack run
stack ghci
```

(Stack erzeugt standardmäßig `app/Main.hs`, `src/Lib.hs`, `test/Spec.hs`.) [7]

---

## 3) Beispielarchitektur + Minimalbeispiele (Woche 1)

**Philosophie**:

* `src/` enthält *pure* Logik (keine Seiteneffekte), leicht testbar.
* `app/Main.hs` nur dünne I/O-Schicht, ruft Funktionen aus `src/` auf. [8]

**Dateien**

`app/Main.hs`

```haskell
module Main where

import Basics (firstElement, swap, comprehensionExample)

main :: IO ()
main = do
  print (firstElement [1,2,3])
  print (swap ("foo", 42))
  print (comprehensionExample 10)
```

`src/Basics.hs`

```haskell
module Basics
  ( firstElement
  , swap
  , comprehensionExample
  ) where

-- | Liefert das erste Element einer Liste
firstElement :: [a] -> a
firstElement = head

-- | Vertauscht die Elemente eines Tupels
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- | Quadrate von 1..n, nur gerade Ergebnisse behalten
comprehensionExample :: Int -> [Int]
comprehensionExample n = [x * x | x <- [1 .. n], even (x * x)]
```

**Warum das zur M323-Zielsetzung passt**

* **Listen**: `firstElement` liest das erste Element.
* **Tupel**: `swap` vertauscht Werte.
* **List Comprehension**: `comprehensionExample` baut und filtert Listen deklarativ.

---

## 4) Produktiver Arbeiten (Fortgeschritten – für später im Modul)

* **REPL‑Komfort** (im Projekt):

```powershell
cabal repl         # oder: stack ghci
-- in GHCi:
:reload            # Neuübersetzen nach Dateiänderung
:t pipeline        # Typ anschauen
:i foldr           # Info/Hintergrund zu Funktionen/Klassen
```

(Details siehe GHCi‑Kapitel im User’s Guide.) [5]

* **Formatter (Fourmolu)**, damit der Code überall gleich aussieht:

```powershell
cabal install fourmolu
fourmolu --mode inplace .
```

Optional eine `fourmolu.yaml` mit `fourmolu --print-defaults > fourmolu.yaml` erzeugen. [9]
In VS Code den Formatter auf „fourmolu“ stellen (Einstellung der Haskell‑Extension). [9]

* **Linting (HLint)**, um einfache Verbesserungen vorzuschlagen:

```powershell
cabal update
cabal install hlint
hlint .
```

(Empfiehlt z. B. übersichtlichere Ausdrücke.) [10]

* **Kleiner Performance‑Tipp**: Für lange Listen kann `foldl'` (streng) aus `Data.List` Speicher sparen:

```haskell
import Data.List (foldl')
fastSum :: [Int] -> Int
fastSum = foldl' (+) 0
```

---

## 5) Häufige Befehls‑Spickzettel

**GHCi (REPL)**

```
ghci                 # starten
:load Datei.hs       # laden
:reload              # neu laden
:type expr           # Typ anzeigen
:info name           # Infos (Typklassen, Definitionen)
:quit                # beenden
```

(Referenz siehe GHC User’s Guide.) [5]

**Cabal**

```
cabal build
cabal run
cabal repl
```

**Stack**

```
stack build
stack run
stack ghci    # = stack repl
```

---

## 6) Troubleshooting (kurz)

* VS Code findet HLS/GHC nicht → `C:\ghcup\bin` in PATH, VS Code neu starten. [1] [4]
* Versionen passen nicht → In `ghcup tui` dieselbe **empfohlene** GHC‑Version aktiv setzen (HLS‑Kompatibilität). [1]

```

### Quellen

[1]: https://www.haskell.org/ghcup/install/ "GHCup Installation"
[2]: https://marketplace.visualstudio.com/items?itemName=haskell.haskell "VS Code Haskell Extension"
[3]: https://cabal.readthedocs.io/en/stable/getting-started.html#windows "Cabal Windows notes"
[4]: https://haskell-language-server.readthedocs.io/en/latest/ "Haskell Language Server documentation"
[5]: https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html#ghci-commands "GHC User's Guide: GHCi commands"
[6]: https://cabal.readthedocs.io/en/stable/cabal-commands.html#cabal-init "cabal init documentation"
[7]: https://docs.haskellstack.org/en/stable/GUIDE/ "Stack user guide"
[8]: https://cabal.readthedocs.io/en/stable/getting-started.html#project-structure "Cabal project structure"
[9]: https://github.com/fourmolu/fourmolu#readme "Fourmolu formatter"
[10]: https://github.com/ndmitchell/hlint#readme "HLint linter"
