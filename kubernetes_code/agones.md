# agones

## docs
[Agones 是什麼？用 Kubernetes 打造開源遊戲伺服器代管平台 (一)](https://blog.gcp.expert/introduce-agones-host-game-server-on-kubernetes-1/)
[Agones 是什麼？用 Kubernetes 打造開源遊戲伺服器代管平台 (二)](https://blog.gcp.expert/introduce-agones-host-game-server-on-kubernetes-2/)

## example code
dockerfile
```
// Dockerfile
FROM debian:stretch
RUN useradd -m server

COPY ./bin/game-server /home/server/game-server
RUN chown -R server /home/server && \
    chmod o+x /home/server/game-server

USER server
ENTRYPOINT ["/home/server/game-server"]
```

gameserver.yaml
``` yaml
gameserver.yaml
apiVersion: "stable.agon.io/v1alpha1"
kind: GameServer
metadata:
  name: my-game-server
spec:
  containerPort: 7654
  # Pod template
  template:
    spec:
      containers:
      - name: my-game-server-container
        image: gcr.io/agon-images/my-game-server:0.1
```

run command:

``` shell
$ kubectl apply -f gamesever.yaml

$ kubectl describe gameserver my-game-server

```

## releated docs
[Installing Agones on OVHcloud Managed Kubernetes](https://docs.ovh.com/ie/en/kubernetes/installing-agones/)

[使用 Game Servers 部署 Xonotic](https://cloud.google.com/architecture/deploying-xonotic-game-servers?hl=zh-cn)

[Running gaming platforms at scale in Kubernetes](https://kasna.com.au/running-gaming-platforms-at-scale-in-kubernetes/)

## Gameserver Ingress Controller
see [Gameserver Ingress Controller](https://github.com/Octops/gameserver-ingress-controller)

## ingress ip

``` shell
EXTERNAL_IP=$(kubectl get services agones-allocator -n agones-system -o jsonpath='{.status.loadBalancer.ingress[0].ip}')
```
copy from [Allocator Service](https://agones.dev/site/docs/advanced/allocator-service/)