if (Test-Path "$env:APPDATA/local/bin/github-release.exe") {
  echo "GitHub Release is already installed."
} else {
  echo "Installing GitHub Release for windows..."
  curl --output github-release.zip --location "https://github.com/tfausak/github-release/releases/download/0.1.9/github-release-0.1.9-windows.zip"
  7z x github-release.zip github-release.exe
  mv github-release.exe "$env:APPDATA/local/bin"
}

github-release version
