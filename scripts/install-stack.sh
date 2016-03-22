#!/bin/sh
set -o errexit -o verbose

mkdir -p "$HOME/.local/bin"
export PATH="$HOME/.local/bin:$PATH"
test ! -f "$HOME/.local/bin/stack"
mkdir -p stack
cd stack
curl --location "https://www.stackage.org/stack/$TRAVIS_OS_NAME-x86_64" > stack.tar.gz
gunzip stack.tar.gz
tar -x -f stack.tar --strip-components 1
cp stack "$HOME/.local/bin/stack"
