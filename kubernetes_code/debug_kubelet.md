# debug kubelet

## check the kubelet status

``` shell
sudo systemctl status kubelet
```

## view the log

```
journalctl -u kubelet --no-pager
```

## fail-swap-on=false

/etc/systemd/system/kubelet.service.d/10-kubeadm.conf
```
Environment="KUBELET_SYSTEM_PODS_ARGS=--pod-manifest-path=/etc/kubernetes/manifests --allow-privileged=true --fail-swap-on=false"
```
then

``` shell
sudo systemctl daemon-reload
systemctl restart kubelet
```

copy from [debug kubelet not starting](https://serverfault.com/questions/877136/debug-kubelet-not-starting)

## disable zram

``` shell
zramctl

swapoff /dev/zram3
echo 3 > /sys/class/zram-control/hot_remove
swapoff /dev/zram2
echo 2 > /sys/class/zram-control/hot_remove
swapoff /dev/zram1
echo 1 > /sys/class/zram-control/hot_remove
swapoff /dev/zram0
echo 0 > /sys/class/zram-control/hot_remove

systemctl disable nvzramconfig.service

systemctl start kubelet
```
copy from [Kubelet启动异常排查](https://cloud-atlas.readthedocs.io/zh_CN/latest/kubernetes/debug/kubelet_start_fail.html)
