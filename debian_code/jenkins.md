#install jenkins in debian jessie

``` shell
wget -q -O - https://pkg.jenkins.io/debian/jenkins.io.key | sudo apt-key add -
```
Then add the following entry in your /etc/apt/sources.list:

``` shell
deb https://pkg.jenkins.io/debian binary/
```

``` shell
apt-get install -t jessie-backports  openjdk-8-jre-headless ca-certificates-java
apt-get install jenkins
```
