# kubernetes example

## create pod

the pod.yml defines the kubernetes pod spec
``` shell
kubectl create -f pod.yml
```

## get the running pod

``` shell
kubectl get pod
docker ps
```

## delete pod

``` shell
kubectl delete pod web-db-pod
```
the `web-db-pod` is defined in the pod.yml

## installation

``` shell
Debian / Ubuntu
apt-get update && apt-get install -y apt-transport-https
curl https://mirrors.aliyun.com/kubernetes/apt/doc/apt-key.gpg | apt-key add -
cat <<EOF >/etc/apt/sources.list.d/kubernetes.list
deb https://mirrors.aliyun.com/kubernetes/apt/ kubernetes-xenial main
EOF
apt-get update
apt-get install -y kubelet kubeadm kubectl
CentOS / RHEL / Fedora
cat <<EOF > /etc/yum.repos.d/kubernetes.repo
[kubernetes]
name=Kubernetes
baseurl=https://mirrors.aliyun.com/kubernetes/yum/repos/kubernetes-el7-x86_64/
enabled=1
gpgcheck=1
repo_gpgcheck=1
gpgkey=https://mirrors.aliyun.com/kubernetes/yum/doc/yum-key.gpg https://mirrors.aliyun.com/kubernetes/yum/doc/rpm-package-key.gpg
EOF
setenforce 0
yum install -y kubelet kubeadm kubectl
systemctl enable kubelet && systemctl start kubelet
```
copy from [OPSX](https://opsx.alibaba.com/mirror)
manual stop service

``` shell
systemctl stop kubelet
systemctl disable kubelet
systemctl stop ebtables.service
systemctl disable ebtables.service
```

## k8s state

```
Pods are 100% volatile, all the time!
You can't make sure a Pod will always have the same IP.
Pods don't have deterministic IPs, but can have deterministic DNS.
Pods always have the same name.
```
copy from [slide](https://codesync.global/uploads/media/default/0001/01/7760ae3859f5d53c9f98b8bbff275d7060f6a806.pdf)

## install the suitable docker-ce

``` shell
export VERSION=18.03 && curl -sSL get.docker.com | sh
```
see [Which kubernetes version is supported in docker version 18.09](https://stackoverflow.com/questions/53256739/which-kubernetes-version-is-supported-in-docker-version-18-09)

## docs
a handy example: [使用kubeadm创建一个K8s 1.10的Cluster](https://zhuanlan.zhihu.com/p/31398416)
[使用kubeadm安装Kubernetes 1.11](https://zhuanlan.zhihu.com/p/40931670)

## init

``` shell
kubeadm init --pod-network-cidr=10.0.2.15/24
```
