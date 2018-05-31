# erlang todo

- [ ] read the net_adm:world/0 functions
- [ ] read inet_res:nslookup/4
- [x] read the `gen_statem` manual and write some examples
- [ ] write code doc
- [ ] write a callback module
- [x] using open network in docker,  a erlang cluster needs a epmd docker container per host
- [x] using bridge network in docker, a erlang cluster needs kubernetes, docker compose, swarn and so on.
- [ ] erlang pid is <A, B, C>. How is it represent the pid.
	  Need to take a look [Can someone explain the structure of a Pid in Erlang?](https://stackoverflow.com/questions/243363/can-someone-explain-the-structure-of-a-pid-in-erlang)
	  [11 External Term Format](http://erlang.org/doc/apps/erts/erl_ext_dist.html)
- [ ] erlang cpu usage
- [ ] erlang and haproxy
	[ranch_proxy_protocol](https://github.com/heroku/ranch_proxy_protocol)
- [x] erlang handle the system signals?
- [ ] erlang parse_transform (used in lager and epipe)
- [ ] tcp message delivery retry
	AMQP protocol might solve this problem.
- [ ] riak_core
- [ ] riak_ensemble
- [x] why riak_ensemble try_commit will spawn in another process?
- [ ] lasp
- [ ] cowboy, cowboy rest callback module
- [x] hackney, just like httpc
- [ ] common test
- [x] dialyzer
- [ ] erlang application prep_stop
- [ ] couchdb
