# elastic stack installation

## install in debian
``` shell
wget -qO - https://artifacts.elastic.co/GPG-KEY-elasticsearch | sudo apt-key add -
sudo apt-get install apt-transport-https
echo "deb https://artifacts.elastic.co/packages/7.x/apt stable main" | sudo tee -a /etc/apt/sources.list.d/elastic-7.x.list

## or install using tsinghua mirror:
echo "deb https://mirrors.tuna.tsinghua.edu.cn/elasticstack/7.x/apt/ stable main" | sudo tee -a /etc/apt/sources.list.d/elastic-7.x.list

sudo apt-get update && sudo apt-get install elasticsearch
```

copy from [Install Elasticsearch with Debian Package](https://www.elastic.co/guide/en/elasticsearch/reference/current/deb.html)

## huawei mirroro
[elasticsearch](https://mirrors.huaweicloud.com/elasticsearch/)

## install elk in debian
[How to Install ELK Stack on Debian 9](https://www.rosehosting.com/blog/how-to-install-the-elk-stack-on-debian-9/)
