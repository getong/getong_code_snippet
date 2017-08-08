# install nginx on debian stretch

## installation
``` shell
$ wget -c https://nginx.org/keys/nginx_signing.key
$ sudo apt-key add nginx_signing.key
$ sudo echo "deb http://nginx.org/packages/debian/ stretch nginx \ndeb-src http://nginx.org/packages/debian/ stretch nginx" > /etc/apt/sources.list.d/nginx.list
$ sudo apt-get update
$ sudo apt-get install -y  nginx
```

## listen a port

``` shell
$ cd /etc/nginx/conf.d
$ cat 8090.conf
server {
        listen 8090;
        charset utf-8;
        location /
        {
                if ($request_filename ~* ^.*?\.(txt|doc|pdf|rar|gz|zip|docx|exe|xlsx|ppt|pptx)$){
                        add_header Content-Disposition: 'attachment;';
                }
                root /backup; #指定实际目录绝对路径；
                        autoindex on;                            #开启目录浏览功能；
                        autoindex_exact_size off;            #关闭详细文件大小统计，让文件大小显示MB，GB单位，默认为b；
                        autoindex_localtime on;              #开启以服务器本地时区显示文件修改日期！

        }
}

```
