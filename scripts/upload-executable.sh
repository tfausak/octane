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
gzip --best --to-stdout "$(stack path --local-install-root)/bin/octane" > "octane-$TRAVIS_OS_NAME.gz"
tar -x --strip-components 3 -f github-release.tar.bz2
./github-release upload \
  --user tfausak \
  --repo octane \
  --tag "$TRAVIS_TAG" \
  --file "octane-$TRAVIS_OS_NAME.gz" \
  --name "octane-$TRAVIS_OS_NAME.gz"
