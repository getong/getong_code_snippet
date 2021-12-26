# compile unreal engine

## compile unreal engine on linux
``` shell
git clone https://github.com/EpicGames/UnrealEngine.git

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
