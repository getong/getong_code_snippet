# docker erlang


## Create cluster of the MongooseIM nodes - multihost setup
> Clustering setup presented in the previous section only makes sense if all containers are on the same on the same node. Otherwise, we won't be able to use links mechanism without extra work(ambassador pattern etc.). The situation is not as bad as it seems to be, the only thing we need to change is switch from --link to proper --add-host options which adds required entries to the /etc/hosts file, Additionally we need to make sure, that we have exposed and forwarded all required ports, which are:

> 4369 - for erlang port mapper daemon
> 9100 - for actual cluster connections
> It was not an issue in the previous case, because by default all ports in the "docker network" are open, so containers are able to talk to each other using them. Below is a sample Vagrant file, which shows how to do that with 2 hosts. See the Mongoose documentation for more details about clustering: http://mongooseim.readthedocs.org/en/latest/operation-and-maintenance/Cluster-configuration-and-node-management/.
