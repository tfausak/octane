set -o errexit -o verbose

if test -f "$HOME/.local/bin/github-release"
then
  echo 'GitHub Release is already installed.'
else
  echo "Installing GitHub Release for $TRAVIS_OS_NAME..."
  V='0.1.9'
  URL="https://github.com/tfausak/github-release/releases/download/$V/github-release-$V-$TRAVIS_OS_NAME.gz"
  curl --location "$URL" > github-release.gz
  gunzip github-release.gz
  mkdir -p "$HOME/.local/bin"
  mv github-release "$HOME/.local/bin/"
fi

github-release version
