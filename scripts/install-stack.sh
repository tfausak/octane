#!/bin/sh
set -o errexit -o verbose

mkdir -p "$HOME/.local/bin"
export PATH="$HOME/.local/bin:$PATH"
test -f "$HOME/.local/bin/stack" ||
  curl -L https://www.stackage.org/stack/linux-x86_64 |
  tar xz --wildcards --strip-components=1 -C "$HOME/.local/bin" '*/stack'
