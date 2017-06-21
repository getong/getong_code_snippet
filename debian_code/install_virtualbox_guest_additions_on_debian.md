# Installing Guest Additions on Debian
See [Installing Guest Additions on Debian](https://virtualboxes.org/doc/installing-guest-additions-on-debian/)
> Login as root;
> Update your APT database with apt-get update;
> Install the latest security updates with apt-get upgrade;
> Install required packages with apt-get install build-essential module-assistant;
> Configure your system for building kernel modules by running m-a prepare;
> Click on Install Guest Additions… from the Devices menu, then run mount /media/cdrom.
> Run sh /media/cdrom/VBoxLinuxAdditions.run, and follow the instructions on screen.

code below:

``` shell
# apt-get update
# apt-get upgrade -y
# apt-get install build-essential module-assistant
# m-a prepare
# sh /media/cdrom/VBoxLinuxAdditions.run
```
