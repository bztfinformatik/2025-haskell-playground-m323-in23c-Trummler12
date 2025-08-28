# M323-Haskell – Developer Notes (Windows 11)

Goal: quick start for module 323 "functional programming" with Haskell.

---

## Install

Zum Projekt-Ordner:

```powershell
cd M323-Haskell   # cd M323-Haskell-COPY
```

- Run GHCup script in PowerShell (no admin):

```powershell
Set-ExecutionPolicy Bypass -Scope Process -Force;
[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;
try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }
```
Nachdem das Begleit-Terminal geschlossen wurde:
- VSCode neu starten
- Installation prüfen:
```powershell
ghcup tui   # install recommended GHC, cabal, HLS
```

- VS Code: install extension `haskell.haskell`.

- Verify tools:

```powershell
ghc --version
cabal --version
ghci --version
```

---

## Basic usage

- Start REPL:
```PowerShell
ghci
```
- Load file:
```ghci
:l simple.hs
:l numlang.hs
-- Repeat :l for every .hs file
:reload
```
- Compile file:
```ghci
:!ghc --make Main.hs -o main.exe
```

---

## Cabal project

```ghci
cabal init --libandexe -n --source-dir=src --application-dir=app --language=GHC2021
cabal build
cabal run
cabal repl
```

---

## Cheat sheet

**GHCi**

```ghci
ghci
:load Datei.hs
:type expr
:info name
:quit
```

**Cabal**

```ghci
cabal build
cabal run
cabal repl
```

---

## Troubleshooting

- VS Code cannot find HLS/GHC → ensure `C:\ghcup\bin` in PATH, then restart VS Code.
- Use `ghcup tui` to switch versions if tools mismatch.
