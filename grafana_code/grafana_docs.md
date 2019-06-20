# grafana docs

## start with docker

``` shell
docker run \
-d \
--name grafana \
-p 3000:3000 \
-v $PWD:/var/lib/grafana \
-e "GF_SECURITY_ADMIN_PASSWORD=ahy8DaeR" \
grafana/grafana:6.2.4
```

## grafana with influxdb example

get the influxdb data sample:
``` shell
curl https://s3.amazonaws.com/noaa.water-database/NOAA_data.txt -o NOAA_data.txt
```

import the data sample:

``` shell
influx -import -path=NOAA_data.txt -precision=s -database=NOAA_water_database
```
Then in the grafana add a new dashbord.
see [Beginning visualization with GRAFANA and INFLUXDB](https://medium.com/@ashrafur/beginning-visualization-with-grafana-and-influxdb-81701e10569d)
