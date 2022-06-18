# compile unreal engine

## compile unreal engine on linux
``` shell
git clone https://github.com/EpicGames/UnrealEngine.git

// or download the tar file from github, for example
// https://github.com/EpicGames/UnrealEngine/archive/refs/tags/4.27.2-release.tar.gz
// tar xzf UnrealEngine-4.27.2-release.tar.gz
// cd UnrealEngine-4.27.2-release

## on archlinux, copy from https://aur.archlinux.org/packages/unreal-engine
yay -S libicu50

cd UnrealEngine
./Setup.sh
./GenerateProjectFiles.sh
make
./Engine/Binaries/Linux/UE4Editor
```
copy from [Building On Linux](https://michaeljcole.github.io/wiki.unrealengine.com/Building_On_Linux/)


## compile unreal engine on macos

```
Fork and Clone the UE4/5 repo, https://github.com/EpicGames/UnrealEngine/tree/5.0.0-early-access-2 47, you want the branch ue5-main
Make sure you have read/write permissions in the directory (FOrk and Clone should take care of it)
Run Setup command, and the Generate Project Files command in the root of fork clone
Update your Project Workspace Settings so it’s the New Build System not the Legacy one, it may be already set
Open Xcode 13, build the ShaderCompileWorker target
build the UE5 project target
Run UE5, and select C++, works fine
```

copy from [UE5 on MacOS Monterey (beta 7)?](https://forums.unrealengine.com/t/ue5-on-macos-monterey-beta-7/252722)

also see [Building Unreal Engine for macOS with XCode](https://medium.com/@lukebrady105/building-unreal-engine-for-macos-with-xcode-bf7f807a65)

## clean unreal engine git repo

``` shell
git clean -d -fx -i
```
copy from [error building Unreal Engine 4.26 on Linux Debian 11 (Bullseye)[(https://answers.unrealengine.com/questions/1017417/view.html)

## ssl error
``` shell
export SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

export SSL_CERT_DIR=/dev/null
./Engine/Build/BatchFiles/RunUAT.sh BuildGraph -target="Make Installed Build Linux" -script=Engine/Build/InstalledEngineBuild.xml -set:WithDDC=false -set:HostPlatformOnly=true
```
copy from [Can't generate project files for UE5 on Linux](https://stackoverflow.com/questions/72539119/cant-generate-project-files-for-ue5-on-linux)
or

``` shell
sudo pacman -S libssl

sudo mkdir /usr/local/ssl
sudo ln -s /etc/ssl/certs /usr/local/ssl

sudo mkdir /usr/lib/ssl
sudo ln -s /etc/ssl/certs /usr/lib/ssl
```
copy from [Error Compiling Unreal Engine on Arch Linux](https://forums.unrealengine.com/t/error-compiling-unreal-engine-on-arch-linux/549637)
