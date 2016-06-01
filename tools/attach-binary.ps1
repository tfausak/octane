if ($env:APPVEYOR_REPO_TAG_NAME) {
  if ($env:GITHUB_TOKEN) {
    echo "Attaching binary for windows to $env:APPVEYOR_REPO_TAG_NAME..."
    stack --local-bin-path . install octane
    7z a octane.zip octane.exe
    github-release.exe upload --token "$env:GITHUB_TOKEN" --owner tfausak --repo octane --tag "$env:APPVEYOR_REPO_TAG_NAME" --file octane.zip --name "octane-$env:APPVEYOR_REPO_TAG_NAME-windows.zip"
  } else {
    echo "The GITHUB_TOKEN environment variable is not set!"
    exit 1
  }
} else {
  echo "This is not a release build."
}
