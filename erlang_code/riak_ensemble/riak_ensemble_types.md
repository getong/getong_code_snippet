# riak_ensemble types

## types are defined in riak_ensemble_types.hrl

``` erlang
-type ensemble_id() :: term().
-type peer_id() :: {term(), node()}.

-record(ensemble_info, {vsn                                   :: vsn(),
                        mod     = riak_ensemble_basic_backend :: module(),
                        args    = []                          :: [any()],
                        leader                                :: leader_id(),
                        views                                 :: [[peer_id()]],
                        seq                                   :: {integer(), integer()}
                       }).

```
