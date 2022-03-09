# nginx config

## subfolders alias include all subfolder

``` nginx
location /maps
    {
        alias /online/www/maps.domain.com/$1;
    }
```
copy from [nginx - subfolders alias include all subfolder](https://serverfault.com/questions/1079230/nginx-subfolders-alias-include-all-subfolder)

## force download file in nginx

``` nginx
location /downloads {
   ...
   add_header Content-disposition "attachment; filename=$1";
   default_type application/octet-stream;
   ...
}
```
or:

``` nginx
location ~* ^/.+\.(?:gif|jpe?g|png|mp4|mp3)$ {
   ...
   add_header Content-disposition "attachment; filename=$1";
   default_type application/octet-stream;
   ...
}
```
then restart nginx:

``` shell
sudo systemctl resetart nginx
```

copy from [How to Force Download File in NGINX](https://fedingo.com/how-to-force-download-file-in-nginx/)
