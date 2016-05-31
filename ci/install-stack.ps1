if (Test-Path "$env:APPDATA/local/bin/stack.exe") {
  echo "Stack is already installed."
} else {
  echo "Installing Stack for windows..."
  curl --output stack.zip --location https://www.stackage.org/stack/windows-x86_64
  7z x stack.zip stack.exe
  mv stack.exe "$env:APPDATA/local/bin"
}

stack --version
