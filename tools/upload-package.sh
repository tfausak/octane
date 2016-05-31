set -o errexit -o verbose

if test ! "$TRAVIS_TAG"
then
  echo 'This is not a release build.'
elif test "$TRAVIS_OS_NAME" != linux
then
  echo 'This build is not being run on Linux.'
else
  echo "Uploading version $TRAVIS_TAG from $TRAVIS_OS_NAME..."
  mkdir -p "$HOME/.stack/upload"
  echo "{ \"username\": \"$HACKAGE_USERNAME\", \"password\": \"$HACKAGE_PASSWORD\" }" > "$HOME/.stack/upload/credentials.json"
  stack upload .
fi
