# debug kubelet

## check the kubelet status

``` shell
sudo systemctl status kubelet
```

## view the log

```
journalctl -u kubelet
```

## fail-swap-on=false

/etc/systemd/system/kubelet.service.d/10-kubeadm.conf
```
Environment="KUBELET_SYSTEM_PODS_ARGS=--pod-manifest-path=/etc/kubernetes/manifests --allow-privileged=true --fail-swap-on=false"
```

copy from [debug kubelet not starting](https://serverfault.com/questions/877136/debug-kubelet-not-starting)
