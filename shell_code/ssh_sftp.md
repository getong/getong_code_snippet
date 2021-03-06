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
#mv id_rsa.pub authorized_keys
#chmod 600 authorized_keys
## delete the id_rsa file
#rm id_rsa


#vi /etc/ssh/sshd_config

PubkeyAuthentication yes ## use key login
AuthorizedKeysFile .ssh/authorized_keys ## key filename

#service sshd restart
```

## keep SSH sessions alive and prevent the SSH timeout
copy from [SSH timeout due to inactivity is annoying. Here’s how to keep your SSH sessions alive and prevent the SSH timeout](https://bjornjohansen.no/ssh-timeout)

``` shell
# Prevent SSH timeout on the client side
# add this in ~/.ssh/config
ServerAliveInterval 120

# Prevent SSH timeout on the server side
# add these two in /etc/ssh/sshd_config
ClientAliveInterval 120
ClientAliveCountMax 720

# and then restart sshd
systemctl restart sshd
```
