># virtualbox

## Fixing your virtualbox shared folder symlink error
windows install virtualbox, and linux guest host has this problem.
see [Fixing your virtualbox shared folder symlink error](https://ahtik.com/fixing-your-virtualbox-shared-folder-symlink-error/)
``` powershell
cd virtualbox_install_dir
VBoxManage setextradata YOURVMNAME VBoxInternal2/SharedFoldersEnableSymlinksCreate/YOURSHAREFOLDERNAME 1

VBoxManage getextradata YOURVMNAME enumerate
```

## normal user can use the shared folder

``` shell
sudo usermod -aG vboxsf $(whoami)
```

## virtualbox can not start on macos 12

Vagrantfile:
```
    config.vm.provider "virtualbox" do |v|
      v.gui = true
    end
```
network config:

``` shell
echo "* 0.0.0.0/0 ::/0" > /usr/local/etc/vbox/networks.conf
```

run the command:

``` shell
sudo kextload -b org.virtualbox.kext.VBoxDrv
sudo kextload -b org.virtualbox.kext.VBoxNetFlt
sudo kextload -b org.virtualbox.kext.VBoxNetAdp
sudo kextload -b org.virtualbox.kext.VBoxUSB
```
copy from [Running Vagrant via “vagrant up” on macOS Monterey (12.0.1) fails](https://apple.stackexchange.com/questions/429609/running-vagrant-via-vagrant-up-on-macos-monterey-12-0-1-fails)
copy from [Cant run virtualbox after updating mac to 12.0.1 monterey - error with host only adapter](https://stackoverflow.com/questions/69839697/cant-run-virtualbox-after-updating-mac-to-12-0-1-monterey-error-with-host-only)

## start virtualbox machine

``` shell
$ VBoxManage list vms

$ VBoxManage startvm XP --type gui

$ VBoxManage list runningvms # 列出运行中的虚拟机
$ VBoxManage controlvm XP acpipowerbutton # 关闭虚拟机，等价于点击系统关闭按钮，正常关机
$ VBoxManage controlvm XP poweroff # 关闭虚拟机，等价于直接关闭电源，非正常关机
$ VBoxManage controlvm XP pause # 暂停虚拟机的运行
$ VBoxManage controlvm XP resume # 恢复暂停的虚拟机
$ VBoxManage controlvm XP savestate # 保存当前虚拟机的运行状态
$ VBoxManage controlvm XP reset # a cold reboot of the virtual machine
```
copy from [使用命令行启动 VirtualBox 虚拟机](https://kodango.com/use-cli-to-start-vm)

## fix install driver error code 2
Install 6.1.26 iso.
It can be download from https://download.virtualbox.org/virtualbox/6.1.26/VBoxGuestAdditions_6.1.26.iso .
copy from [Error al instalar las Guest Additions en Virtualbox : Exit code 2](https://linuxmanr4.com/2022/02/17/error-al-instalar-las-guest-additions-en-virtualbox-exit-code-2/)

## modifymedium

``` shell
VBoxManage modifymedium imageFile --resize  newSizeMb

// for example, 100Gb disk
// VBoxManage modifymedium win7.vdi --resize  102400

// or clone it, and modify it
VBoxManage clonemedium centos7-4G.vmdk centos7-4G.vdi --format VDI
VBoxManage modifymedium centos7-4G.vdi --resize 10000
```
copy from [VirturalBox 调整虚拟磁盘大小](https://zangchuantao.com/tech-zh/2021/virturalbox-adjust-storage-size/)
