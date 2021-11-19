# virtualbox

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
