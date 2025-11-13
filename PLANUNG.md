# Planung zur Umstrukturierung des Submodules "M323-Haskell"

Involviertes Lehr-Modul:
- Modul 323: Funktional programmieren

## Vorbereitung

### Prompt 1

<prompt1>
Wir besuchen in der Lehre ein Modul "Modul 323: Funktional programmieren" und so hab' ich in meinem Main-Repo für dieses Schul-Modul ein entsprechendes Submodule (M323-Haskell);
Ich hab' die Aufgaben bisher alle im Root des Submodules umgesetzt, aber mittlerweile wird's eher etwas unübersichtlich;
Daher wollte ich dich fragen, wie eine typische Haskell-Architektur aussieht;
So sieht's momentan aus:
```
M323-Haskell 
|   .ghci
|   GHCiPrint.hs
|   local-tree.ps1
|   local-tree.txt
|   numlang.hs
|   README.md
|   simple.hs
|   vector.hs
|
\---.github
        .keep
```
Und so hätte ich gedacht, *könnte* es vielleicht aussehen (wenn ich die Übungen einfach modularisieren wollen würde):
```
M323-Haskell  
|   .ghci
|   local-tree.ps1
|   local-tree.txt
|   README.md
|
+---.github
|       .keep
|
+---demo
|       .ghci
|       simple.hs
|
+---geometry
|   |   .ghci
|   |   filterByOrthants.hs
|   |   vector.hs
|   |
|   \---shapes
|       +---2d
|       |       .ghci
|       |       circle.hs
|       |       square.hs
|       |       triangle.hs
|       |
|       \---3d
|               .ghci
|               cube.hs
|               sphere.hs
|
+---numlang
|       .ghci
|       numlang.hs
|
\---utility
        .ghci
        GHCiPrint.hs
```
Und dann kann man im obersten .ghci (oder vlt. auch über einen anderen Weg) sagen, was man alles laden möchte; Dabei würde man entweder einzelne Files oder (über deren .ghci) funktionale Gruppen ansteuern können - mit der grundlegenden Idee, dass irgendwann mal erstellte Klassen bei Bedarf jederzeit wiederverwendet werden können (vlt. auch mit einer Logik, dass wenn man circle.hs laden möchte, Abhängigkeiten wie vector.hs dann automatisch ebenfalls geladen werden; Kann man das direkt im File circle.hs definieren?);
Würd's das so in der Art Sinn ergeben?
Aber natürlich wäre ich - mit Verweis auf Java-Projekte - nicht überrascht, wenn auch *hier* die empfohlene Architektur "etwas" komplexer ist als man vielleicht erwarten würde
</prompt1>

### Antwort 1

<antwort1>
Kurzfassung vorab: Statt vielen `.ghci`-Dateien lohnt es sich, die Übungen als saubere Haskell-Pakete mit Cabal/Stack zu strukturieren (ein Repo = Workspace, darin 1..n Pakete). Abhängigkeiten werden **durch `import` + `.cabal`** gelöst, nicht durch `.ghci`. Dann lädst du zielgerichtet mit `cabal repl <komponente>` genau die Gruppe, an der du arbeitest.

# Empfohlene Struktur (ein Paket, viele Module)

Für ein Schul-Repo mit wiederverwendbaren Bausteinen reicht oft ein **einzelnes Paket** mit klarer Modul-Hierarchie:

```
M323-Haskell/
|-- m323-haskell.cabal
|-- cabal.project              # optional, gut für Workspaces
|-- README.md
|-- app/
|   |-- demo.hs                # "main" fürs Rumprobieren
|   `-- numlang-app.hs         # falls numlang ein Executable ist
|-- src/
|   |-- Demo/
|   |   `-- Simple.hs
|   |-- Geometry/
|   |   |-- Vector.hs
|   |   |-- FilterByOrthants.hs
|   |   `-- Shapes/
|   |       |-- TwoD/
|   |       |   |-- Circle.hs
|   |       |   |-- Square.hs
|   |       |   `-- Triangle.hs
|   |       `-- ThreeD/
|   |           |-- Cube.hs
|   |           `-- Sphere.hs
|   `-- NumLang/
|       `-- NumLang.hs
|-- test/
|   `-- Spec.hs                # QuickCheck/Hspec
`-- bench/
    `-- Bench.hs               # Criterion optional
```

**Warum so?**

* **Module = Ordnerpfade.** `Geometry.Shapes.TwoD.Circle` liegt in `src/Geometry/Shapes/TwoD/Circle.hs`.
* **Keine `.ghci` nötig.** Du startest zielgerichtete REPLs je Komponente:
  `cabal repl` (Library), `cabal repl exe:demo`, `cabal repl test:tests`.
* **Automatische Abhängigkeiten.** Wenn `Circle` `Vector` braucht, importierst du `Geometry.Vector`. Cabal baut die Reihenfolge selbst; nichts “manuell in GHCi laden”.

## Minimal-`m323-haskell.cabal` (Beispiel)

```cabal
cabal-version:      >=1.24
name:               m323-haskell
version:            0.1.0.0
build-type:         Simple

common common-opts
  default-language: Haskell2010
  ghc-options:      -Wall -Wcompat -Widentities -Wincomplete-uni-patterns -Wredundant-constraints
  default-extensions: OverloadedStrings
  build-depends:    base >=4.14 && <5, text

library
  import:           common-opts
  hs-source-dirs:   src
  exposed-modules:
      Demo.Simple
      Geometry.Vector
      Geometry.FilterByOrthants
      Geometry.Shapes.TwoD.Circle
      Geometry.Shapes.TwoD.Square
      Geometry.Shapes.TwoD.Triangle
      Geometry.Shapes.ThreeD.Cube
      Geometry.Shapes.ThreeD.Sphere
      NumLang.NumLang
  other-modules:    -- leer lassen oder nicht-exportierte Module aufführen

executable demo
  import:           common-opts
  main-is:          demo.hs
  hs-source-dirs:   app
  build-depends:    base, text, m323-haskell

executable numlang-app
  import:           common-opts
  main-is:          numlang-app.hs
  hs-source-dirs:   app
  build-depends:    base, text, m323-haskell

test-suite tests
  import:           common-opts
  type:             exitcode-stdio-1.0
  hs-source-dirs:   test
  main-is:          Spec.hs
  build-depends:    base, text, QuickCheck, m323-haskell
```

> Hinweis: Willst du “funktionale Gruppen” separat laden, nutze **mehrere Komponenten** im selben Paket (z. B. zusätzliche `internal-libraries`) oder **mehrere Pakete** im Workspace (siehe unten).

## Beispiel-Module und Imports

`src/Geometry/Shapes/TwoD/Circle.hs`

```haskell
module Geometry.Shapes.TwoD.Circle
  ( Circle(..)
  , area
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Geometry.Vector (Vec2(..), norm)

newtype Circle = Circle { radius :: Double }

area :: Circle -> Double
area (Circle r) = pi * r * r

describe :: Circle -> Text
describe c = T.pack ("Circle r=" <> show (radius c))
```

`src/Geometry/Vector.hs`

```haskell
module Geometry.Vector
  ( Vec2(..)
  , norm
  ) where

data Vec2 = Vec2 !Double !Double

norm :: Vec2 -> Double
norm (Vec2 x y) = sqrt (x*x + y*y)
```

**Wichtig zur Frage “automatisch laden”:**
Du definierst **keine** Abhängigkeiten “im File” (wie in Java `import` vs. Build-Tool). In Haskell reicht der `import` im Modul; Cabal/Stack liest die Modul-Liste aus der `.cabal` und baut/linkt automatisch. GHCi lädt transitiv alles, was für das Ziel nötig ist, wenn du über `cabal repl` startest.

## Typischer Workflow

* **Bauen/Tests:** `cabal build`, `cabal test`
* **Gezielte REPLs:**

  * Ganze Library: `cabal repl`
  * Demo-Executable: `cabal repl exe:demo`
  * Nur Tests: `cabal repl test:tests`
* **Einzelfile-Hacks:** kurzzeitig `runghc app/demo.hs` ist ok – aber lieber “richtig” über `cabal repl`.

# Alternative: Workspace mit mehreren Paketen

Wenn “NumLang” und “Geometry” wirklich unabhängig sind [WAS DER FALL IST!], splitte in **Pakete**:

```
M323-Haskell/
|-- cabal.project
|-- geometry/
|   |-- geometry.cabal
|   `-- src/Geometry/...
|-- numlang/
|   |-- numlang.cabal
|   `-- src/NumLang/...
`-- demos/
    |-- demos.cabal
    `-- app/demo.hs
```

`cabal.project`:

```cabal
packages:
  geometry/
  numlang/
  demos/
```

Dann:

* `cabal repl geometry`
* `cabal repl numlang`
* `demos` kann von `geometry` und `numlang` abhängen (in `demos.cabal` via `build-depends: geometry, numlang`).

# Migration (deine jetzigen Files → saubere Struktur)

Format nach deinem bevorzugten Schema “WHERE / WHAT / WHY”.

**1) WHERE**
Root: `M323-Haskell/`

**WHAT**

* Lege `src/`, `app/`, `test/` an.
* Verschiebe:

  * `vector.hs` → `src/Geometry/Vector.hs` + `module Geometry.Vector where`
  * `simple.hs` → `src/Demo/Simple.hs` + `module Demo.Simple where`
  * `numlang.hs` → `src/NumLang/NumLang.hs` + `module NumLang.NumLang where`
  * `GHCiPrint.hs` (falls Hilfsfunktionen) → `src/Utility/GHCiPrint.hs` + `module Utility.GHCiPrint where`
* Erzeuge `app/demo.hs` mit `main` (importiert `Demo.Simple`).
* Erzeuge `m323-haskell.cabal` (siehe oben).
* Entferne projektspezifische `.ghci`-Dateien (optional behalten, aber unnötig).

**WHY**

* Einheitliche Modulnamen ↔ Pfade, weniger Chaos.
* Reproduzierbare Builds; transitive Abhängigkeiten ohne manuelles Laden.
* Klare Orte für Demos (`app/`), Bibliothekscode (`src/`), Tests (`test/`).

**2) WHERE**
`circle.hs` und Co.

**WHAT**

* `circle.hs` → `src/Geometry/Shapes/TwoD/Circle.hs`
  Ganz oben: `module Geometry.Shapes.TwoD.Circle (...) where`
  Dann `import Geometry.Vector`.
* `square.hs`, `triangle.hs` analog; 3D-Formen nach `Shapes/ThreeD/`.

**WHY**

* Wiederverwendbarkeit über stabile Modulpfade; klare Semantik 2D vs. 3D.

**3) WHERE**
Root Build/REPL-Bedienung.

**WHAT**

* Nutze `cabal repl` statt `.ghci`.
* Definiere bei Bedarf interne Libraries, um Gruppen separat zu laden, z. B.:

```cabal
library geometry
  import:         common-opts
  hs-source-dirs: src
  exposed-modules:
    Geometry.Vector
    Geometry.FilterByOrthants
    Geometry.Shapes.TwoD.Circle
    -- …

library numlang
  import:         common-opts
  hs-source-dirs: src
  exposed-modules:
    NumLang.NumLang

executable demo
  import:         common-opts
  main-is:        demo.hs
  hs-source-dirs: app
  build-depends:  base, text, geometry, numlang
```

* Laden: `cabal repl lib:geometry`, `cabal repl lib:numlang`.

**WHY**

* Präzise, schnelle REPLs für Teilmengen ohne Querschnittslast.

# Kleine Qualitätsregeln

* **`Text` statt `String`** in öffentlichen APIs (per `OverloadedStrings` + `text`).
* **Explizite Exporte** in jedem Modul: kontrolliert API-Fläche.
* **`-Wall`** u. strikte Warnungen im Cabal-Common-Block.
* **Tests**: QuickCheck-Eigenschaften pro Modul (z. B. `area (Circle r) >= 0`).
* **Benennung**: Module groß, Funktionen klein; keine Dateinamen im CamelCase abweichend vom Modul.

# Antworten auf deine Kernfragen

* **“Kann ich in `circle.hs` Abhängigkeiten definieren, die dann automatisch geladen werden?”**
  **Ja, durch `import …`**. Der Build lädt transitiv alles Nötige. Es braucht keine `.ghci`-Magie.
* **“Macht die vorgeschlagene Ordnerstruktur Sinn?”**
  **Ja**, wenn du sie als **Modul-/Paketstruktur** mit Cabal umsetzt (statt `.ghci`-basiert). Für rein manuelles Laden ist es ok, aber du verlierst Reproduzierbarkeit.
* **“Ist Haskell-Architektur komplex (wie Java)?”**
  Im Gegenteil: **einfacher**. Üblich sind `src/`, `app/`, `test/`, `bench/` plus klare Modulpfade. Komplexität kommt erst bei Multi-Paketen/Monorepos – Cabal/Stack handhaben das sauber.

Wenn du möchtest, erstelle ich dir sofort eine maßgeschneiderte `.cabal` plus Platzhalterdateien auf Basis deiner aktuellen Dateien (inkl. passender `module`-Header).
</antwort1>

### Prompt 2

<prompt2>
Please again ultrathink hard on this:
"Wenn “NumLang” und “Geometry” wirklich unabhängig sind, splitte in **Pakete**:"
=> Wenn man diese Gruppen in "Pakete" splitten würde, würde das bedeuten, dass diese Pakete auch niemals zusammen geladen werden können?
Rein funktional wären diese 3 Gruppen (numlang, demos und geometry) sehr wohl "unabhängig" voneinander, also würden niemals (nativ) untereinander kommunizieren, aber
ich würde halt schon gerne im Terminal sowohl numlang als auch geometry ad hoc ansteuern können wollen.
Kann es sein, dass ich mir gerade zu viele Sorgen mache?
</prompt2>

### Antwort 2

<antwort2>
Kurz: Ja, du kannst mehrere **Pakete** gleichzeitig in einer REPL haben. Das Aufsplitten in Pakete heißt *nicht*, dass sie nie zusammen ladbar sind. Sie bleiben nur baulich entkoppelt. Es gibt dafür drei saubere Wege (Cabal), plus eine Stack-Variante.

# Optionen, um `geometry` und `numlang` ad hoc zusammen zu laden

## 1) Cabal „Scratch-REPL“ (ohne extra Ziel)

Einfach beide Pakete als Build-Depends angeben:

```bash
cabal repl --build-depends 'geometry, numlang'
```

Dann in GHCi:

```haskell
import Geometry.Vector
import NumLang.NumLang
```

Voraussetzung: Die betroffenen Module sind im jeweiligen Paket unter `exposed-modules` (oder via `reexported-modules`) sichtbar.

**Wofür gut?** Schnell, kein Zusatzcode nötig.
**Beachte:** Cabal erzeugt dafür on-the-fly ein temporäres Ziel; das ist Standard-Workflow.

## 2) Aggregator-Ziel (kleines Dev-Executable oder -Library)

Lege im Repo ein Mini-Paket/Executable an (z. B. `demos`) mit

```cabal
executable demos
  hs-source-dirs: app
  main-is:        DemoMain.hs
  build-depends:  base, geometry, numlang
```

Start:

```bash
cabal repl exe:demos
```

In `DemoMain.hs` kannst du beliebig importieren oder die REPL so starten und frei importieren.

**Wofür gut?** Reproduzierbar, teamtauglich, schnell wiederverwendbar.
**Beachte:** Winzige Boilerplate-Datei, dafür klare Doku im Repo.

## 3) Interne Libraries pro Themenblock

In einem *einzigen* Paket mehrere **internal libraries** definieren, z. B. `lib:geometry` und `lib:numlang`, und ein `exe`/`test` hängt an beide:

```bash
cabal repl lib:geometry
cabal repl lib:numlang
cabal repl exe:demos     # lädt transitiv beide
```

**Wofür gut?** Feingranulare REPLs, schneller Inkrementalbuild.
**Beachte:** Alles bleibt im selben Paket; sauber, wenn die Trennung eher „logisch“ als „distributiv“ ist.

## 4) Stack (falls du Stack nutzt)

```bash
stack ghci geometry:lib numlang:lib
```

Stack kann mehrere Targets direkt in eine Session laden.

---

## Antworten auf deine Sorgen

* **„Pakete = nie gemeinsam laden?“** Falsch. Du kannst sie gemeinsam in **einer** REPL haben (Option 1/2/4). Sie kommunizieren nur, wenn du es willst (d. h. wenn dein REPL-Ziel von beiden abhängt oder du beide als Build-Depends angibst).
* **„Mache ich mir zu viele Sorgen?“** Ein bisschen. Die Toolchain (Cabal/Stack) ist genau dafür gebaut. Entkopplung durch Pakete ist gut; gleichzeitige Arbeit an beiden ist trivial.

## Kleine Checkliste, damit es reibungslos klappt

* In jedem Paket: die Module, die du in der REPL sehen willst, in `exposed-modules` aufführen. Nicht-öffentliche Helfer in `other-modules`.
* Keine Modulnamens-Kollisionen zwischen Paketen (gleichnamige Module sind *möglich*, aber verwirrend).
* `cabal.project` im Monorepo listet alle Pakete:

  ```cabal
  packages:
    geometry/
    numlang/
    demos/
  ```

Wenn du magst, skizziere ich dir fix eine schlanke `cabal.project` + drei minimalistische `.cabal`-Files (geometry/numlang/demos) mit `exposed-modules`, sodass `cabal repl --build-depends 'geometry, numlang'` und `cabal repl exe:demos` sofort funktionieren.
</antwort2>


