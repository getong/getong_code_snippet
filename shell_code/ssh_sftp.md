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

## ssh Forwarding other ports

Local forwarding is accomplished by means of the -L switch and it is accompanying forwarding specification in the form of <tunnel port>:<destination address>:<destination port>.

``` shell
ssh -L 1000:mail.google.com:25 192.168.0.100
```
will use SSH to login to and open a shell on 192.168.0.100, and will also create a tunnel from the local machine's TCP port 1000 to mail.google.com on port 25.
Once established, connections to localhost:1000 will connect to the Gmail SMTP port. To Google,
it will appear that any such connection (though not necessarily the data conveyed over the connection) originated from 192.168.0.100,
and such data will be secure between the local machine and 192.168.0.100, but not between 192.168.0.100 and Google, unless other measures are taken.

copy from [Forwarding_other_ports](https://wiki.archlinux.org/title/OpenSSH#Forwarding_other_ports)
also see [转发其他端口](https://wiki.archlinux.org/title/OpenSSH_(%E7%AE%80%E4%BD%93%E4%B8%AD%E6%96%87))


the `~/.ssh/config` :

``` shell
Host example
    HostName ip_address
    User host_name
    LocalForward 7474 127.0.0.1:7474
    LocalForward 7777 127.0.0.1:7777
```
then run the command:

``` shell
ssh example
```
copy from [SSH 配置端口转发文件~/.ssh/config](https://www.codeleading.com/article/61621764082/)

also see [彻底搞懂SSH端口转发命令](https://zhuanlan.zhihu.com/p/148825449)

## remote ssh port forwarding

``` shell
sudo vim /etc/ssh/sshd_config
-----------------------------------
GatewayPorts yes

sudo systemctl restart sshd
```

Example One:

``` shell
// A host:
ssh -R 22122:10.0.2.16:22 laptop_user@192.168.1.233

// B host:
sftp -P 22122 virtual_user@localhost
```
copy from [彻底搞懂SSH端口转发命令](https://zhuanlan.zhihu.com/p/148825449)

Example Two:

``` shell
// host3
ssh -R 2121:host2:21 host1

// host1
ftp localhost:2121
```
让host1监听它自己的2121端口，然后将所有数据经由host3，转发到host2的21端口。
copy from [SSH原理与运用（二）：远程操作与端口转发](https://www.ruanyifeng.com/blog/2011/12/ssh_port_forwarding.html)
