# kube-apiserver

## setting

>>>
How do I turn on an admission controller?
The Kubernetes API server flag enable-admission-plugins takes a comma-delimited list of admission control plugins to invoke prior to modifying objects in the cluster. For example, the following command line enables the NamespaceLifecycle and the LimitRanger admission control plugins:

``` shell
kube-apiserver --enable-admission-plugins=NamespaceLifecycle,LimitRanger ...
```
>>>
Note: Depending on the way your Kubernetes cluster is deployed and how the API server is started, you may need to apply the settings in different ways. For example, you may have to modify the systemd unit file if the API server is deployed as a systemd service, you may modify the manifest file for the API server if Kubernetes is deployed in a self-hosted way.

copy from [Using Admission Controllers](https://kubernetes.io/docs/reference/access-authn-authz/admission-controllers/#validatingadmissionwebhook-alpha-in-18-beta-in-19)

The config file is located in `/etc/kubernetes/manifests/kube-apiserver.yaml`
