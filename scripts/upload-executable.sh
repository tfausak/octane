#!/bin/sh
set -o errexit -o verbose

test "$TRAVIS_TAG"
test "$GITHUB_TOKEN"
if test "$TRAVIS_OS_NAME" = 'linux'
then
  curl --location 'https://github.com/aktau/github-release/releases/download/v0.6.2/linux-amd64-github-release.tar.bz2' > github-release.tar.bz2
else
  if test "$TRAVIS_OS_NAME" = 'osx'
  then
    curl --location 'https://github.com/aktau/github-release/releases/download/v0.6.2/darwin-amd64-github-release.tar.bz2' > github-release.tar.bz2
  fi
fi
tar -x --strip-components 3 -f github-release.tar.bz2
./github-release upload \
  --user tfausak \
  --repo octane \
  --tag "$TRAVIS_TAG" \
  --name "octane-$TRAVIS_OS_NAME" \
  --file "$(stack path --local-install-root)/bin/octane"
