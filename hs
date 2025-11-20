#!/usr/bin/env bash
set -euo pipefail

mapfile -t CABALS < <(find . -maxdepth 2 -type f -name '*.cabal' \
  -not -path './dist-newstyle/*' | sort)

ALL_PKGS=()
LIB_PKGS=()
for cabal in "${CABALS[@]}"; do
  name=$(awk 'BEGIN{IGNORECASE=1} $1=="name:" {print $2; exit}' "$cabal")
  [[ -n "${name:-}" ]] || continue
  ALL_PKGS+=("$name")
  if awk 'BEGIN{IGNORECASE=1} /^library([[:space:]]|$)/{found=1} END{exit !found}' "$cabal"; then
    LIB_PKGS+=("$name")
  fi
done

# Normalize path arguments like "./numlang/" to "numlang"
normalizeTarget() {
  local t="$1"
  # Strip leading ./ or .\
  t="${t#./}"
  t="${t#.\\}"
  # Strip trailing slashes
  t="${t%/}"
  t="${t%\\}"
  printf '%s\n' "$t"
}

rootGhci="$(pwd)/.ghci"
ghciScriptFor() {
  local pkg="${1:-}"
  # Aggregator (empty pkg) -> .ghci-all (all imports)
  # Single-lib (one pkg)   -> .ghci (neutral, Cabal sets context)
  local p; [[ -n "$pkg" ]] && p=".ghci" || p=".ghci-all"
  [[ -f "$p" ]] && printf '%s\n' "$(pwd)/$p" || printf '%s\n' "$rootGhci"
}

if [[ "${1:-}" == "--list" ]]; then
  echo "Library packages: ${LIB_PKGS[*]}"
  echo "All packages: ${ALL_PKGS[*]}"
  exit 0
fi

if [[ $# -eq 0 ]]; then
  script="$(ghciScriptFor)"
  if [[ -f demos/demos.cabal ]]; then
    exec cabal repl demos:exe:demo --repl-options "-ghci-script=$script"
  elif [[ ${#LIB_PKGS[@]} -gt 0 ]]; then
    IFS=, exec cabal repl --build-depends "${LIB_PKGS[*]}" --repl-options "-ghci-script=$script"
  else
    echo "Keine Library-Pakete gefunden." >&2; exit 1
  fi
else
  # Normalize first argument
  normalized="$(normalizeTarget "$1")"
  if printf '%s\n' "${ALL_PKGS[@]}" | grep -qx -- "$normalized"; then
    script="$(ghciScriptFor "$normalized")"
    exec cabal repl "lib:$normalized" --repl-options "-ghci-script=$script"
  else
    # Multi-repl or unknown: pass through with neutral .ghci
    exec cabal repl "$@" --repl-options "-ghci-script=$rootGhci"
  fi
fi
