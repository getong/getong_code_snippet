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
>
> Debian - /etc/rabbitmq/
>
> RPM - /etc/rabbitmq/
>
>  Mac OS X (Homebrew) - ${install_prefix}/etc/rabbitmq/, the Homebrew prefix is usually /usr/local
>
> Windows - %APPDATA%\RabbitMQ\
>
> If rabbitmq-env.conf doesn't exist, it can be created manually in the location, specified by the RABBITMQ_CONF_ENV_FILE variable. On Windows systems, it is named rabbitmq-env.bat.
>
> If rabbitmq.config doesn't exist, it can be created manually. Set the RABBITMQ_CONFIG_FILE environment variable if you change the location. The Erlang runtime automatically appends the .config extension to the value of this variable.
>
> Restart the server after changes. Windows service users will need to re-install the service after adding or removing a configuration file.

See the reference [RabbitMQ Configuration](https://www.rabbitmq.com/configure.html)

## docker start rabbitmq

``` shell
docker pull rabbitmq:3.7.2
docker run -d --privileged --restart=always --network host --name rabbitmq_server rabbitmq:3.7.2
```

## docker rabbitmq config file
[How to configure rabbitmq.config inside Docker containers?](https://stackoverflow.com/questions/42003640/how-to-configure-rabbitmq-config-inside-docker-containers)

> the config file lives in /etc/rabbitmq/rabbitmq.config so if you mount your own config file

## monitor rabbitmq connection
[Module amqp_connection](https://www.rabbitmq.com/releases/rabbitmq-erlang-client/v3.6.14/doc/)
> Please note that connections and channels do not get restarted automatically by the supervision tree in the case of a failure.
> If you need robust connections and channels, we recommend you use Erlang monitors on the returned connection and channel PIDs.

[monitor rabbitmq code snippet](https://gist.github.com/burinov/4287139)
