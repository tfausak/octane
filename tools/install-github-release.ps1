if (Test-Path C:\bin\github-release.exe) {
  echo "GitHub Release is already installed."
} else {
  echo "Installing GitHub Release for windows..."
  curl -OutFile github-release.zip -Uri "https://github.com/tfausak/github-release/releases/download/0.1.9/github-release-0.1.9-windows.zip"
  7z x github-release.zip github-release.exe
  mkdir C:\bin
  mv github-release.exe C:\bin
}

github-release.exe version
