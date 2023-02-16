# flutter installation

## macos set java sdk
If already have android studio installed simply run:
```shell
# add this into ~/.zshrc file
export JAVA_HOME="/Applications/Android Studio.app/Contents/jbr/Contents/Home"

## install cmd
cd ~/Library/Android/sdk/tools/bin
./sdkmanager --install "cmdline-tools;latest"

## set sdk path
flutter config --android-sdk "/Users/$USER/Library/Android/sdk" & flutter doctor --android-licenses

## check result
flutter doctor
```
copy from https://stackoverflow.com/questions/68236007/i-am-getting-error-cmdline-tools-component-is-missing-after-installing-flutter
