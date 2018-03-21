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
