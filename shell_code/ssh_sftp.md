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
