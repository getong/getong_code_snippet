# modify local language
## server side setting
```
$cp /etc/default/locale /etc/default/locale.bak
$vim /etc/default/locale
LANG="zh_CN.UTF-8"
LANGUAGE="zh_CN:zh"

$locale-gen zh_CN.UTF-8
```
reboot and then it works.

## desktop side

```
dpkg-reconfigure locales

```
choose the utf-8 language, and reboot.
