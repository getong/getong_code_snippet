# kubeadm setup kubernetes cluster

## get images list

``` shell
kubeadm config images list

images=(  # 下面的镜像应该去除"k8s.gcr.io/"的前缀，版本换成上面获取到的版本
    kube-apiserver:v1.24.2
    kube-controller-manager:v1.24.2
    kube-scheduler:v1.24.2
    kube-proxy:v1.24.2
    pause:3.7
    etcd:3.3.5.3-0
    coredns:v1.8.6
)

for imageName in ${images[@]} ; do
    docker pull registry.cn-hangzhou.aliyuncs.com/google_containers/$imageName
    docker tag registry.cn-hangzhou.aliyuncs.com/google_containers/$imageName k8s.gcr.io/$imageName
    docker rmi registry.cn-hangzhou.aliyuncs.com/google_containers/$imageName
done
```

## init

``` shell
kubeadm init # 这一步注意，如果需要特定的网络插件，需要额外加参数，具体看网络插件的介绍
```

## auth

``` shell
mkdir -p $HOME/.kube
sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config
sudo chown $(id -u):$(id -g) $HOME/.kube/config
```

## network plugin

``` shell
mkdir -p /etc/cni/net.d
cat >/etc/cni/net.d/10-mynet.conf <<-EOF
{
    "cniVersion": "0.3.0",
    "name": "mynet",
    "type": "bridge",
    "bridge": "cni0",
    "isGateway": true,
    "ipMasq": true,
    "ipam": {
        "type": "host-local",
        "subnet": "10.244.0.0/16",
        "routes": [
            {"dst": "0.0.0.0/0"}
        ]
    }
}
EOF
cat >/etc/cni/net.d/99-loopback.conf <<-EOF
{
    "cniVersion": "0.3.0",
    "type": "loopback"
}
EOF
```

## flannel

``` shell
kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/v0.10.0/Documentation/kube-flannel.yml
```

## weave

``` shell
sysctl net.bridge.bridge-nf-call-iptables=1
kubectl apply -f "https://cloud.weave.works/k8s/net?k8s-version=$(kubectl version | base64 | tr -d '\n')"
```

## calico

``` shell
kubectl apply -f https://docs.projectcalico.org/v3.1/getting-started/kubernetes/installation/hosted/rbac-kdd.yaml
kubectl apply -f https://docs.projectcalico.org/v3.1/getting-started/kubernetes/installation/hosted/kubernetes-datastore/calico-networking/1.7/calico.yaml

```

## set kubernetes master node can also run pod

``` shell
kubectl taint nodes --all node-role.kubernetes.io/master-
```

## get other images

``` shell
将yaml文件中镜像地址的k8s.gcr.io替换成registry.cn-hangzhou.aliyuncs.com/google_containers
```

## check the cluster running status

``` shell
kubectl get pods -n kube-system
NAME                              READY   STATUS              RESTARTS   AGE
coredns-86c58d9df4-mmjls          1/1     Running             0          6h26m
coredns-86c58d9df4-p7brk          1/1     Running             0          6h26m
etcd-promote                      1/1     Running             1          6h26m
kube-apiserver-promote            1/1     Running             1          6h26m
kube-controller-manager-promote   1/1     Running             1          6h25m
kube-proxy-6ml6w                  1/1     Running             1          6h26m
kube-scheduler-promote            1/1     Running             1          6h25m
```

copy from [kubernetes安装（国内环境）](https://zhuanlan.zhihu.com/p/46341911)
