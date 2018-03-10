# windows exchange the caps and ctrl key
copy from [WINDOWS Ctrl 与 CapsLock 互换--注册表修改](https://www.jianshu.com/p/4a062318f334)

save this in exchange.reg and execute it.

```
Windows Registry Editor Version 5.00

[HKEY_LOCAL_MACHINE\SYSTEM\CurrentControlSet\Control\Keyboard Layout]
"Scancode Map"=hex:00,00,00,00,00,00,00,00,03,00,00,00,3a,00,1d,00,1d,00,3a,00,\
  00,00,00,00

```
