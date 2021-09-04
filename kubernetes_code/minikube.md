# minikube

copy from [你好，Minikube](https://kubernetes.io/zh/docs/tutorials/hello-minikube/)

## 启动 Minikube 并创建一个集群：

``` shell
minikube start
```

## 创建 Deployment

Kubernetes Pod 是由一个或多个 为了管理和联网而绑定在一起的容器构成的组。
本教程中的 Pod 只有一个容器。
Kubernetes Deployment 检查 Pod 的健康状况，并在 Pod 中的容器终止的情况下重新启动新的容器。
Deployment 是管理 Pod 创建和扩展的推荐方法。

### 使用 kubectl create 命令创建管理 Pod 的 Deployment。该 Pod 根据提供的 Docker 镜像运行 Container。

``` shell
kubectl create deployment hello-minikube --image=k8s.gcr.io/echoserver:1.10
```

### 查看 Deployment：

``` shell
$ kubectl get deployments
NAME         READY   UP-TO-DATE   AVAILABLE   AGE
hello-node   1/1     1            1           1m
```

### 查看 Pod：

``` shell
$ kubectl get pods
NAME                          READY     STATUS    RESTARTS   AGE
hello-node-5f76cf6ccf-br9b5   1/1       Running   0          1m
```

### 查看集群事件：

``` shell
kubectl get events
```

### 查看 kubectl 配置：

``` shell
kubectl config view
```

### hello-minikube发布为服务以连接到部署：

``` shell
kubectl expose deployment hello-minikube --type=NodePort --port=8080
```

### hello-minikubePod 已经开始启动，但您需要等待 Pod 完成启动，然后才能通过已发布的 Service 进行连接。

检查 pod 是否正在运行：

``` shell
kubectl get pod
```
`STATUS` 在`ContainerCreating` 显示的情况下，Pod 仍在构建中：

```
hello-minikube-3383150820-vctvh   0/1       ContainerCreating   0          3s
```
`STATUS` 如果Running显示 ，则 pod 已启动并正在运行：

```
NAME                              READY     STATUS    RESTARTS   AGE
hello-minikube-3383150820-vctvh   1/1       Running   0          13s
```

### 获取发布的Service的URL，查看Service的详细信息：

``` shell
minikube service hello-minikube --url
```

### hello-minikube删除服务：

``` shell
$ kubectl delete services hello-minikube
service "hello-minikube" deleted
```

### hello-minikube删除部署：

``` shell
$ kubectl delete deployment hello-minikube
deployment.extensions "hello-minikube" deleted
```

### 删除本地 Minikube 集群：

``` shell
$ minikube delete
Deleting "minikube" ...
The "minikube" cluster has been deleted.
```

## 创建 Service
默认情况下，Pod 只能通过 Kubernetes 集群中的内部 IP 地址访问。 要使得 hello-node 容器可以从 Kubernetes 虚拟网络的外部访问，你必须将 Pod 暴露为 Kubernetes Service。

### 使用 kubectl expose 命令将 Pod 暴露给公网：

``` shell
kubectl expose deployment hello-node --type=LoadBalancer --port=8080
```
这里的 --type=LoadBalancer 参数表明你希望将你的 Service 暴露到集群外部。

镜像 k8s.gcr.io/echoserver 中的应用程序代码仅监听 TCP 8080 端口。 如果你用 kubectl expose 暴露了其它的端口，客户端将不能访问其它端口。

### 查看你创建的 Service：

``` shell
$ kubectl get services
NAME         TYPE           CLUSTER-IP      EXTERNAL-IP   PORT(S)          AGE
hello-node   LoadBalancer   10.108.144.78   <pending>     8080:30369/TCP   21s
kubernetes   ClusterIP      10.96.0.1       <none>        443/TCP          23m
```

### 运行下面的命令：

``` shell
minikube service hello-node
```

## 启用插件

### 列出当前支持的插件：

``` shell
$ minikube addons list
|-----------------------------|----------|--------------|
|         ADDON NAME          | PROFILE  |    STATUS    |
|-----------------------------|----------|--------------|
| ambassador                  | minikube | disabled     |
| auto-pause                  | minikube | disabled     |
| csi-hostpath-driver         | minikube | disabled     |
| dashboard                   | minikube | disabled     |
| default-storageclass        | minikube | enabled ✅   |
| efk                         | minikube | disabled     |
| freshpod                    | minikube | disabled     |
| gcp-auth                    | minikube | disabled     |
| gvisor                      | minikube | disabled     |
| helm-tiller                 | minikube | disabled     |
| ingress                     | minikube | disabled     |
| ingress-dns                 | minikube | disabled     |
| istio                       | minikube | disabled     |
| istio-provisioner           | minikube | disabled     |
| kubevirt                    | minikube | disabled     |
| logviewer                   | minikube | disabled     |
| metallb                     | minikube | disabled     |
| metrics-server              | minikube | disabled     |
| nvidia-driver-installer     | minikube | disabled     |
| nvidia-gpu-device-plugin    | minikube | disabled     |
| olm                         | minikube | disabled     |
| pod-security-policy         | minikube | disabled     |
| registry                    | minikube | disabled     |
| registry-aliases            | minikube | disabled     |
| registry-creds              | minikube | disabled     |
| storage-provisioner         | minikube | enabled ✅   |
| storage-provisioner-gluster | minikube | disabled     |
| volumesnapshots             | minikube | disabled     |
|-----------------------------|----------|--------------|
```

### 启用插件，例如 metrics-server：

``` shell
minikube addons enable metrics-server
```

### 查看创建的 Pod 和 Service：

``` shell
$ kubectl get pod,svc -n kube-system
NAME                                   READY   STATUS             RESTARTS   AGE
pod/coredns-7f89b7bc75-6zrz8           0/1     Running            0          173m
pod/etcd-minikube                      1/1     Running            0          173m
pod/kube-apiserver-minikube            1/1     Running            0          173m
pod/kube-controller-manager-minikube   1/1     Running            0          173m
pod/kube-proxy-vgt6j                   0/1     CrashLoopBackOff   38         173m
pod/kube-scheduler-minikube            1/1     Running            0          173m
pod/storage-provisioner                0/1     CrashLoopBackOff   38         173m

NAME               TYPE        CLUSTER-IP   EXTERNAL-IP   PORT(S)                  AGE
service/kube-dns   ClusterIP   10.96.0.10   <none>        53/UDP,53/TCP,9153/TCP   173m
```

### 禁用 metrics-server：

``` shell
$ minikube addons disable metrics-server
metrics-server was successfully disabled
```

## 清理
现在可以清理你在集群中创建的资源：

``` shell
kubectl delete service hello-node
kubectl delete deployment hello-node
```

可选地，停止 Minikube 虚拟机（VM）：

``` shell
minikube stop
```

可选地，删除 Minikube 虚拟机（VM）：

``` shell
minikube delete
```

## image

``` shell
minikube image load image_name

minikube image list
```
