# ssh sftp终端使用

## 确保key的权限为400

```
chmod 400 id_rsa
```

## ssh with key

```
ssh -i id_rsa user@ip
```

## sftp with key, use as ftp

```
sftp -i id_rsa user@ip
```

## scp拷贝文件到远程服务器

```
scp -i id_rsa file user@ip:/tmp/dir
scp -i id_rsa dir/* user@host:/var/cache/yum/x86_64/7/updates/packages/
```

## 拷贝目录到远程服务器

```
scp -r local_folder remote_username@remote_ip:remote_folder
```

## 拷贝目录到本地

```
scp user@ip:/tmp/file /tmp
```

## user login via ssh in a different port other than 22

```
ssh user@host -p port
```

## enable public key login

``` shell
#mkdir ~/.ssh
#chmod 755 ~/.ssh
#ssh-keygen -t rsa  # enter the password of the key, not the user's password
#cd ~/.ssh
#cp id_rsa.pub authorized_keys
#chmod 600 authorized_keys
#chmod 600 id_rsa

#vi /etc/ssh/sshd_config

PubkeyAuthentication yes ## use key login
AuthorizedKeysFile .ssh/authorized_keys ## key filename

#service sshd restart
```
