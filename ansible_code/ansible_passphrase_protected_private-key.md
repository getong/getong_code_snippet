# ansible work with passphrase protected private-key

## ssh-agent, expect solution
`expect` need to install.
see [How to make ssh-add read passphrase from a file?](https://stackoverflow.com/questions/13033799/how-to-make-ssh-add-read-passphrase-from-a-file)

``` shell
#!/bin/bash

if [ $# -ne 2 ] ; then
  echo "Usage: ssh-add-pass keyfile passfile"
  exit 1
fi

eval $(ssh-agent -s)
pass=$(cat $2)

expect << EOF
  spawn ssh-add $1
  expect "Enter passphrase"
  send "$pass\r"
  expect eof
EOF
```
If work with many other jobs, the `eval $(ssh-agent -s)` must be divided into other part.
1. start the ssh-agent

``` shell
$ eval $(ssh-agent -s)
```

2. modify the ssh-add-pass script file

``` shell
#!/bin/sh

if [ $# -ne 2 ] ; then
  echo "Usage: ssh-add-pass keyfile passfile"
  exit 1
fi

pass=$(cat $2)

expect << EOF
  spawn ssh-add $1
  expect "Enter passphrase"
  send "$pass\r"
  expect eof
EOF
```

3. call the ssh-add-pass script file

``` shell
$ chmod +x ssh-add-pass
$ ./ssh-add-pass id_rsa passfile
```
the `id_rsa` file is the key private file, the passfile contains the passphrase

4. on the same shell, to do more jobs. Like ssh connect to the remote server

``` shell
$ ssh user@192.168.1.1 -i id_rsa
$ scp -i id_rsa file user@192.168.1.1:/home/user
```
That is it.

## ansible plugin solution
see [ansible/ansible#22764](https://github.com/ansible/ansible/pull/22764)
It is a pr need to be merged(now time is 2017-12-07).
This might be work.
