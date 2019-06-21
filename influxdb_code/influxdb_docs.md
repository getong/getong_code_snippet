# influxdb docs

## InfluxDB中文文档
[InfluxDB中文文档](https://jasper-zhang1.gitbooks.io/influxdb/)

## influx

``` shell
$ docker run --rm influxdb:1.7.6 influxd config  > influxdb.conf
$ docker run -d  -p 8087:8086 -v $PWD/influxdb.conf:/etc/influxdb/influxdb.conf:ro influxdb -config /etc/influxdb/influxdb.conf
$ influx -port 8087
```


## erlang driver
[influx_udp](https://github.com/palkan/influx_udp)
[exometer_influxdb](https://github.com/travelping/exometer_influxdb)
