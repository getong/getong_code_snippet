# podman

## generante kube

```
podman generate kube
```
copy from [Exploring Podman and Kubernetes Interoperability on RHEL](https://acloudguru.com/hands-on-labs/exploring-podman-and-kubernetes-interoperability-on-rhel)

or also

```
sudo podman run -dt -p 8000:80 --name demo quay.io/libpod/alpine_nginx:latest

sudo podman ps
 sudo podman generate kube demo  > demo.yml

 sudo kubectl create -f demo.yml

 sudo kubectl get pods

 sudo podman pod ps
```
copy from [Podman can now ease the transition to Kubernetes and CRI-O](https://developers.redhat.com/blog/2019/01/29/podman-kubernetes-yaml#)
