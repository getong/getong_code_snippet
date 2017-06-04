# erlang use macro to avoid wrong spell

Using `macro` to define an atom or a number in case of wrong spell and give it a full meaning.

```
%% copy from erlang array.erl
-define(DEFAULT, undefined).

-define(LEAFSIZE, 10).| |   % the "base"
```
Like the number macro, if it is uesed in many places, when it has to be changed, edit the definition, it changes all.

But when the atom is used in msg as the tuple tag, no need to used as macro.

## normal_reason
``` erlang
-define(normal_reason(R),
        (R == normal orelse R == shutdown orelse
                                            (is_tuple(R) andalso element(1,R) == shutdown))).
```
copy from riak_core_vnode.erl
