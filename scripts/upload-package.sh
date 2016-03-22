#!/bin/sh
set -o errexit -o verbose

test "$TRAVIS_TAG"
test "$TRAVIS_OS_NAME" = 'linux'
mkdir -p "$HOME/.stack/upload"
echo "{ \"username\": \"$HACKAGE_USERNAME\", \"password\": \"$HACKAGE_PASSWORD\" }" > "$HOME/.stack/upload/credentials.json"
stack upload .
