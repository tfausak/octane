if (Test-Path C:\bin\stack.exe) {
  echo "Stack is already installed."
} else {
  echo "Installing Stack for windows..."
  curl -OutFile stack.zip -Uri https://www.stackage.org/stack/windows-x86_64
  7z x stack.zip stack.exe
  mkdir C:\bin
  mv stack.exe C:\bin
}

stack.exe --version
