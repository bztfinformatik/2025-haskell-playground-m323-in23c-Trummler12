# What needs to be done before using the project

## PowerShell Profile

**Path**: `C:\Users\<Username>\Documents\WindowsPowerShell\Microsoft.PowerShell_profile.ps1`

```Powershell
# PowerShell Profile - Custom Functions

# hs - Haskell REPL wrapper for M323-Haskell project
# Automatically calls hs.ps1 from the current directory if it exists
function hs {
  param([Parameter(ValueFromRemainingArguments = $true)][string[]]$PassThru)

  $localScript = Join-Path $PWD 'hs.ps1'
  if (Test-Path $localScript) {
    # Filter out empty strings and call appropriately
    $cleanArgs = @($PassThru | Where-Object { $_ -and $_.Trim() })
    if ($cleanArgs.Count -gt 0) {
      & $localScript @cleanArgs
    } else {
      & $localScript
    }
  }
  else {
    Write-Error "Kein hs.ps1 im aktuellen Verzeichnis gefunden."
    Write-Host "Hinweis: Diese Funktion funktioniert nur im M323-Haskell-Projektordner." -ForegroundColor Yellow
  }
}

Write-Host "PowerShell Profile geladen. ''hs'' Funktion verfugbar fur M323-Haskell." -ForegroundColor Green
```
