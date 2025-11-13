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

ghciScriptFor() {
  local pkg="${1:-}"
  local p; [[ -n "$pkg" ]] && p=".ghci-$pkg" || p=".ghci-all"
  [[ -f "$p" ]] && printf '%s\n' "$p" || printf '%s\n' ".ghci"
}

if [[ "${1:-}" == "--list" ]]; then
  echo "Library packages: ${LIB_PKGS[*]}"
  echo "All packages: ${ALL_PKGS[*]}"
  exit 0
fi

if [[ $# -eq 0 ]]; then
  script="$(ghciScriptFor)"
  if [[ -f demos/demos.cabal ]]; then
    exec cabal repl demos:exe:demo --repl-options "-ghci-script $script"
  elif [[ ${#LIB_PKGS[@]} -gt 0 ]]; then
    IFS=, exec cabal repl --build-depends "${LIB_PKGS[*]}" --repl-options "-ghci-script $script"
  else
    echo "Keine Library-Pakete gefunden." >&2; exit 1
  fi
else
  if printf '%s\n' "${ALL_PKGS[@]}" | grep -qx -- "$1"; then
    script="$(ghciScriptFor "$1")"
    exec cabal repl "lib:$1" --repl-options "-ghci-script $script"
  else
    exec cabal repl "$@"
  fi
fi
