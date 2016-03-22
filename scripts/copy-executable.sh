#!/bin/sh
set -o errexit -o verbose

mkdir -p tmp
cp "$(stack path --local-install-root)/bin/octane" "tmp/octane"
