if (Test-Path C:\bin\github-release.exe) {
  echo "GitHub Release is already installed."
} else {
  echo "Installing GitHub Release for windows..."
  curl -OutFile github-release.zip -Uri "https://github.com/tfausak/github-release/releases/download/1.0.5/github-release-1.0.5-windows.zip"
  7z x github-release.zip github-release.exe
  mkdir C:\bin
  mv github-release.exe C:\bin
}

github-release.exe version
