if ($env:APPVEYOR_REPO_TAG_NAME) {
  if ($end:GITHUB_TOKEN) {
    echo "Attaching binary for windows to $env:APPVEYOR_REPO_TAG_NAME..."
    7z a octane.zip "$(./stack.exe path --local-install-root)/bin/octane"
    github-release.exe upload --token "$env:GITHUB_TOKEN" --owner tfausak --repo octane --tag "$env:APPVEYOR_REPO_TAG_NAME" --file octane.zip --name "octane-$env:APPVEYOR_REPO_TAG_NAME-windows.zip"
  } Else {
    echo "The GITHUB_TOKEN environment variable is not set!"
    exit 1
  }
} Else {
  echo "This is not a release build."
}
