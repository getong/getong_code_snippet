[scala官网](www.scala-lang.org) 下载的deb包严重依赖 Oracle JDK, 不支持OpenJDK，而且最好是在Ubuntu系统下使用比较好。
因为它在找JAVA_HOME目录下面的release文件，但OpenJDK没有这个文件。

下载scala for unix [scala-2.12.1.tgz](http://downloads.lightbend.com/scala/2.12.1/scala-2.12.1.tgz)

解压到/usr/local/就可以了。
在PATH环境变量中添加Oracle JDK就可以了
```
export JAVA_HOME=/usr/local/jdk1.8.0_112
export PATH=$JAVA_HOME/bin:$PATH
export PATH=/usr/local/scala-2.12.1/bin:$PATH
```
