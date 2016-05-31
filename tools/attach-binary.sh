set -o errexit -o verbose

if test ! "$TRAVIS_TAG"
then
  echo 'This is not a release build.'
elif test ! "$GITHUB_TOKEN"
then
  echo 'The GITHUB_TOKEN environment variable is not set!'
  exit 1
else
  echo "Attaching binary for $TRAVIS_OS_NAME to $TRAVIS_TAG..."
  BIN="$(stack path --local-install-root)/bin/octane"
  gzip --best --to-stdout "$BIN" > octane.gz
  github-release upload \
    --token "$GITHUB_TOKEN" \
    --owner tfausak \
    --repo octane \
    --tag "$TRAVIS_TAG" \
    --file octane.gz \
    --name "octane-$TRAVIS_TAG-$TRAVIS_OS_NAME.gz"
fi
