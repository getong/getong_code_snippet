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

## archlinux install flutter
``` shell
pacman -S --needed git base-devel
git clone https://aur.archlinux.org/yay.git
cd yay
makepkg -si

yay -S flutter

java -version

sudo pacman -S jre8-openjdk

export JAVA_HOME='/usr/lib/jvm/java-8-openjdk'
export PATH=$JAVA_HOME/bin:$PATH


sudo groupadd flutterusers
sudo gpasswd -a $USER flutterusers
sudo chown -R :flutterusers /opt/flutter
sudo chmod -R g+w /opt/flutter/

yay -S android-sdk android-sdk-platform-tools android-sdk-build-tools
yay -S android-platform

sudo groupadd android-sdk
sudo gpasswd -a $USER android-sdk
sudo setfacl -R -m g:android-sdk:rwx /opt/android-sdk
sudo setfacl -d -m g:android-sdk:rwX /opt/android-sdk

sdkmanager --install "system-images;android-29;default;x86"

avdmanager create avd -n <name> -k "system-images;android-29;default;x86"

export ANDROID_SDK_ROOT='/opt/android-sdk'
export PATH=$PATH:$ANDROID_SDK_ROOT/platform-tools/
export PATH=$PATH:$ANDROID_SDK_ROOT/tools/bin/
export PATH=$PATH:$ANDROID_ROOT/emulator
export PATH=$PATH:$ANDROID_SDK_ROOT/tools/


flutter doctor --android-licenses

flutter create new_app
cd new_app
flutter run --debug
```
copy from https://gist.github.com/joaquinicolas/b7d0a0869485bca5156d0d4be87820f4
also see [Configure Flutter development environment on Manjaro/Arch linux.](https://dev.to/awais/configure-flutter-development-environment-on-manjaro-arch-linux-4a0a)
also see [How to Get Flutter and Android Working on Arch Linux](https://www.rockyourcode.com/how-to-get-flutter-and-android-working-on-arch-linux/)


## dart repl package
``` shell
$ dart pub global activate interactive

$ echo 'export PATH="$HOME/.pub-cache/bin":"$PATH"' >> ~/.zshrc
$ source ~/.zshrc

$ interactive
```

interactive usage
``` dart
>>> class A {
...     late int val;
... }
>>> a = A()
Instance of 'A'
>>> a.val = 999;
>>> print(a.val);
999

>>> !pwd
/tmp/dart_interactive_workspace_2022-11-02T145529907367

>>> !ls
lib
pubspec.yaml

>>> !dart pub add crypto
Resolving dependencies...

+ collection 1.17.0
+ crypto 3.0.2
+ typed_data 1.3.1

Changed 3 dependencies!

>>> import 'package:crypto/crypto.dart';
>>> import 'dart:convert';
>>> b = utf8.encode('helloworld');
>>> d = sha1.convert(b);
>>> d
6adfb183a4a2c94a2f92dab5ade762a47889a5a1
```
copy from [Flutterメモ-30 (dart の REPL)(interactive)](https://devlights.hatenablog.com/entry/2022/11/04/073000)