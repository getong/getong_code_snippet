# dart installation

## flutter
[flutter](flutter.cn)

## download link
https://mirrors.tuna.tsinghua.edu.cn/flutter
``` shell
wget -c https://mirrors.tuna.tsinghua.edu.cn/flutter/flutter_infra/releases/stable/linux/flutter_linux_2.8.0-stable.tar.xz
wget -c https://mirrors.tuna.tsinghua.edu.cn/flutter/flutter_infra/releases/stable/macos/flutter_macos_2.8.0-stable.zip
wget -c https://mirrors.tuna.tsinghua.edu.cn/flutter/dart-archive/channels/stable/release/2.15.0/sdk/dartsdk-linux-x64-release.zip

## or
## wget -c https://storage.flutter-io.cn/dart-archive/channels/stable/release/2.15.0/sdk/dartsdk-macos-x64-release.zip


wget -c https://storage.flutter-io.cn/flutter_infra/releases/stable/macos/flutter_macos_2.8.0-stable.zip
wget -c https://storage.flutter-io.cn/dart-archive/channels/stable/release/2.15.0/api-docs/dartdocs-gen-api.zip

wget -c https://storage.googleapis.com/flutter_infra/releases/stable/macos/flutter_macos_2.8.0-stable.zip
wget -c https://storage.googleapis.com/dart-archive/channels/stable/release/2.15.0/api-docs/dartdocs-gen-api.zip
```
get the download link from [Dart SDK archive](https://dart.dev/tools/sdk/archive)

## android studio

```
https://developer.android.google.cn/studio
```

## pub install package

``` shell
pub global active package_names
```

## dart packages
[dart packages](https://pub.flutter-io.cn/)


## install flutter in archlinux

``` shell
sudo pacman -S flutter android-tools kotlin dart android-studio android-emulator android-sdk
sudo gpasswd -a $USER flutterusers
newgrp flutterusers
```
copy from [Start Flutter 2 in Arch Linux](https://dev.to/nabbisen/start-flutter-2-in-arch-linux-4ab6)
