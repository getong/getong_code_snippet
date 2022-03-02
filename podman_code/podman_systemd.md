# podmon and systemd

## start podmon container after system boot

``` shell
podman run --rm --detach \
    --name toms-mongo \
    --volume /var/lib/mongo:/data/db:Z \
    docker.io/library/mongo

podman ps
podman generate systemd \
    --new --name toms-mongo \
    | sudo tee /etc/systemd/system/toms-mongo.service


systemctl enable --now toms-mongo

systemctl status toms-mongo
```
copy from [How to Start Containers Automatically, with Podman and Systemd](https://www.tutorialworks.com/podman-systemd/)
