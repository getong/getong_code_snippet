# rabbitmq

## debian source list install

``` shell
echo 'deb http://www.rabbitmq.com/debian/ testing main' |
     sudo tee /etc/apt/sources.list.d/rabbitmq.list

wget -O- https://www.rabbitmq.com/rabbitmq-release-signing-key.asc |
     sudo apt-key add -

sudo apt-get update

sudo apt-get install rabbitmq-server
```

## debian client tutorial
[rabbitmq-tutorials](https://github.com/rabbitmq/rabbitmq-tutorials)

## "guest" user can only connect via localhost

set the rabbitmq.config :

``` shell
[{rabbit, [{loopback_users, []}]}].
```
See the reference [Access Control (Authentication, Authorisation) in RabbitMQ](https://www.rabbitmq.com/access-control.html)

## Location of rabbitmq.config and rabbitmq-env.conf
> Generic UNIX - $RABBITMQ_HOME/etc/rabbitmq/
> Debian - /etc/rabbitmq/
> RPM - /etc/rabbitmq/
> Mac OS X (Homebrew) - ${install_prefix}/etc/rabbitmq/, the Homebrew prefix is usually /usr/local
> Windows - %APPDATA%\RabbitMQ\

>If rabbitmq-env.conf doesn't exist, it can be created manually in the location, specified by the RABBITMQ_CONF_ENV_FILE variable. On Windows systems, it is named rabbitmq-env.bat.

>If rabbitmq.config doesn't exist, it can be created manually. Set the RABBITMQ_CONFIG_FILE environment variable if you change the location. The Erlang runtime automatically appends the .config extension to the value of this variable.

>Restart the server after changes. Windows service users will need to re-install the service after adding or removing a configuration file.

See the reference [RabbitMQ Configuration](https://www.rabbitmq.com/configure.html)
