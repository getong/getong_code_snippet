# flutter cmd

```
## detector the fluter environment
flutter doctor

## update flutter sdk
flutter upgrade

## get the packages
flutter packages get

## update the packages
flutter packages upgrade

## create flutter application
flutter create --no-pub my_app

## get the dependency
cd my_app
export PUB_HOSTED_URL=https://pub.flutter-io.cn
export FLUTTER_STORAGE_BASE_URL=https://storage.flutter-io.cn
flutter pub get

## on mac, open a simulator
open -a Simulator

## run the flutter application
flutter run
```

## fluter key functions

``` dart
Widget.build/1 function will build the UI.
setState/1 function will rebuild the UI.
createState/0 function will create the State.
initState/0 function will init the State.
```
