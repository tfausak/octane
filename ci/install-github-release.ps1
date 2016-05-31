if (Test-Path "$env:APPDATA/local/bin/github-release.exe") {
  echo "GitHub Release is already installed."
} else {
  echo "Installing GitHub Release for windows..."
  curl -OutFile github-release.zip -Uri "https://github.com/tfausak/github-release/releases/download/0.1.9/github-release-0.1.9-windows.zip"
  7z x github-release.zip github-release.exe
  mkdir "$env:APPDATA/local/bin"
  mv github-release.exe "$env:APPDATA/local/bin"
}

github-release.exe version
