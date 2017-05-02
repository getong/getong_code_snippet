# manually install java as default
See the [How do I make java default to a manually installed JRE/JDK?](https://askubuntu.com/questions/159575/how-do-i-make-java-default-to-a-manually-installed-jre-jdk)

Use the code:

``` shell
# Adding a new alternative for "java".
sudo update-alternatives --install /usr/bin/java java /usr/local/jdk1.8.0_131/bin/java 1

# Setting the new alternative as default for "java".
sudo update-alternatives --config java
```

Note that, `/usr/local/jdk1.8.0_131/bin/java` is my local install dir, change as your install dir.

More than java, this might be apply to many other softwares.
