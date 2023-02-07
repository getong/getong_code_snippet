# php installation

## install nginx and php in centos 7

set nginx repo
```shell
vim /etc/yum.repos.d/nginx.repo
[nginx-stable]
name=nginx stable repo
baseurl=http://nginx.org/packages/centos/$releasever/$basearch/
gpgcheck=1
enabled=1
gpgkey=https://nginx.org/keys/nginx_signing.key
module_hotfixes=true

[nginx-mainline]
name=nginx mainline repo
baseurl=http://nginx.org/packages/mainline/centos/$releasever/$basearch/
gpgcheck=1
enabled=0
gpgkey=https://nginx.org/keys/nginx_signing.key
module_hotfixes=true
```

install nginx
``` shell
yum install nginx
systemctl start nginx
systemctl enable nginx
```

configure a website
``` shell
vim/etc/nginx/conf.d/www.baidu.com.conf

server {
    listen	80;
    server_name  www.baidu.com baidu.com;
        root	/home/baidu;
        index index.html index.htm index.php;

        location ~ .php$ {
        try_files $uri =404;
        root /home/baidu;
        # fastcgi_pass 127.0.0.1:9000;
       # 若使用file socket則改成下面这行
        fastcgi_pass   unix:/var/run/php-fpm/php-fpm.sock;
        fastcgi_index index.php;
        fastcgi_param SCRIPT_FILENAME $document_root$fastcgi_script_name;
        include fastcgi_params;
    }
}
```
install php
``` shell
yum install php php-ctype php-json php-openssl php-nette-tokenizer \
    php-pecl-zip php-pdo php-mbstring php-xml php-mysql php-fpm

```
configure php files:
```shell
vim /etc/php.ini
cgi.fix_pathinfo=0

vim /etc/php-fpm.d/www.conf
user = nginx
group = nginx

user = nginx
group = nginx
listen = /var/run/php-fpm/php-fpm.sock

systemctl enable php-fpm
systemctl start php-fpm
```
copy from [CentOS 7上安装并配置Nginx、PHP、MySql](https://zhuanlan.zhihu.com/p/121746437)
also see [於CentOS7安裝 Nginx + php7 + php-fpm + Laravel5.6](https://ivanagyro.medium.com/%E6%96%BCcentos7%E5%AE%89%E8%A3%9D-nginx-php7-php-fpm-laravel5-6-df8631681acf)
also see [CentOS 7 配置 nginx php-fpm 详细教程](https://www.cnblogs.com/ryanzheng/p/11263261.html)
