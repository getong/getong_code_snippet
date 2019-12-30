# gpg verify file
The `.sig`, `.asc` file is the signature file, use the `gpg` command line the verify it.
## get the key

```
gpg --keyserver keyserver.ubuntu.com --recv-keys  03D6E495
```

## verify the file

```
gpg --verify julia-0.5.1-full.tar.gz.asc julia-0.5.1-full.tar.gz
```

## restart the gpg-agent

```
gpgconf --kill gpg-agent
```
## gpg
```
## gpg 2.1.17 above, generate a gpg key
gpg --full-generate-key

gpg --default-new-key-algo rsa4096 --gen-key

## list gpg keys
gpg --list-secret-keys --keyid-format LONG
gpg --list-secret-keys

## export key
gpg --armor --export pub-GPG-KEY-ID

## use below
gpg --export ${ID} > public.key
gpg --export-secret-key ${ID} > private.key

## import key
gpg --import public.key
gpg --import private.key
gpg --edit-key {KEY} trust quit
```
copy from [How to import secret gpg key (copied from one machine to another)?](https://unix.stackexchange.com/questions/184947/how-to-import-secret-gpg-key-copied-from-one-machine-to-another)

## gpg Inappropriate ioctl for device

``` shell
echo "allow-loopback-pinentry" >> ~/.gnupg/gpg-agent.conf
echo "use-agent
pinentry-mode loopback" >> ~/.gnupg/gpg.conf
```
copy from [gpg: 签名时失败处理](https://blog.csdn.net/wenbo20182/article/details/72850810)

## set the gpg key pasword ttl

``` shell
echo "default-cache-ttl 3600" >> ~/.gnupg/gpg-agent.conf
```
copy from [Remember GPG password when signing git commits](https://stackoverflow.com/questions/36847431/remember-gpg-password-when-signing-git-commits)

## use  console-based prompt and reload gpg setting
copy from [How to force GPG to use console-mode pinentry to prompt for passwords?](https://superuser.com/questions/520980/how-to-force-gpg-to-use-console-mode-pinentry-to-prompt-for-passwords)
``` shell
echo "pinentry-program /usr/bin/pinentry-tty" >> ~/.gnupg/gpg-agent.conf
gpg-connect-agent reloadagent /bye
```
