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
