# nginx config

## subfolders alias include all subfolder

``` nginx
location /maps
    {
        alias /online/www/maps.domain.com/$1;
    }
```
copy from [nginx - subfolders alias include all subfolder](https://serverfault.com/questions/1079230/nginx-subfolders-alias-include-all-subfolder)
