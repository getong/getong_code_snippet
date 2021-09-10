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

## reset

``` shell
kubeadm reset
```

## kubeadm troubleshooting
see [对 kubeadm 进行故障排查](https://kubernetes.io/zh/docs/setup/production-environment/tools/kubeadm/troubleshooting-kubeadm/)

```
无法通过其服务 IP 访问 Pod
许多网络附加组件尚未启用 hairpin 模式 该模式允许 Pod 通过其服务 IP 进行访问。这是与 CNI 有关的问题。 请与网络附加组件提供商联系，以获取他们所提供的 hairpin 模式的最新状态。

如果你正在使用 VirtualBox (直接使用或者通过 Vagrant 使用)，你需要 确保 hostname -i 返回一个可路由的 IP 地址。默认情况下，第一个接口连接不能路由的仅主机网络。 解决方法是修改 /etc/hosts，请参考示例 Vagrantfile。

```

## exec command

``` shell
## get pods
kubectl get pods -A -o wide

kubectl exec -it pod_name -n namespace -- /bin/sh

## if a pod has many containers:
## copy from [exec](https://kubernetes.io/docs/reference/generated/kubectl/kubectl-commands#exec)
kubectl exec (POD | TYPE/NAME) [-c CONTAINER] [flags] -- COMMAND [args...]
```
copy from [获取正在运行容器的 Shell](https://kubernetes.io/zh/docs/tasks/debug-application-cluster/get-shell-running-container/)

## kubeadm init
192.168.2.1 is localhost ip, 172.17.0.1/16 is docker interface broadcast subnetwork, 10.244.0.0/16 is flannel subnetwork.
The command is executed as root.
``` shell
# kubeadm init \
  --apiserver-advertise-address=192.168.2.1 \
  --service-cidr=172.17.0.1/16 \
  --pod-network-cidr=10.244.0.0/16 \
  --image-repository='registry.cn-hangzhou.aliyuncs.com/google_containers'
```

## patch service external ip

``` shell
## node's IPs
kubectl patch svc <svc-name> -n <namespace> -p '{"spec": {"type": "LoadBalancer", "externalIPs":["172.31.71.218"]}}'
```
copy from [Kubernetes service external ip pending](https://stackoverflow.com/questions/44110876/kubernetes-service-external-ip-pending)

## debug kubernetes service
see [调试 Service](https://kubernetes.io/zh/docs/tasks/debug-application-cluster/debug-service/)
see [Service](https://kubernetes.io/docs/concepts/services-networking/service)
see [Kubernetes集群中访问LoadBalancer暴露出去的SLB地址不通](https://help.aliyun.com/document_detail/171437.html)
see [服务发现与负载均衡](https://jimmysong.io/kubernetes-handbook/practice/service-discovery-and-loadbalancing.html)

## external ip
see [公开外部 IP 地址以访问集群中应用程序](https://kubernetes.io/zh/docs/tutorials/stateless-application/expose-external-ip-address/)

## install ingress-nginx

``` shell
kubectl apply -f https://raw.githubusercontent.com/kubernetes/ingress-nginx/controller-v1.0.0/deploy/static/provider/baremetal/deploy.yaml
```

copy from [Installation Guide](https://kubernetes.github.io/ingress-nginx/deploy/#bare-metal)

## ingress
see [Nginx-ingress 控制器到底怎样实现的，这篇文章教你看明白了](https://zhuanlan.zhihu.com/p/406571145)
