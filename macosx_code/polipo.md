# polipo

## install

``` shell
brew install polipo
```

## configure the ~/.polipo

```
socksParentProxy = "127.0.0.1:1080"
socksProxyType = socks5
proxyAddress = "::0"        # both IPv4 and IPv6
# or IPv4 only
# proxyAddress = "0.0.0.0"
proxyPort = 8123

```

## start polipo

``` shell
brew start polipo
```

## view the start status

open the browser and view http://127.0.0.1:8123/polipo/config
