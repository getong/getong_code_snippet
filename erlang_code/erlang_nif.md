#erlang nif loading
>NIF libraries are loaded by calling erlang:load_nif/2, with the name of the shared library as argument. The second argument can be any term that will be passed on to the library and used for initialization:

```
-module(complex6).
-export([foo/1, bar/1]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./complex6_nif", 0).

foo(_X) ->
    exit(nif_library_not_loaded).
bar(_Y) ->
    exit(nif_library_not_loaded).
```

>Here, the directive on_load is used to get function init to be automatically called when the module is loaded. If init returns anything other than ok, such when the loading of the NIF library fails in this example, the module is unloaded and calls to functions within it, fail.

>Loading the NIF library overrides the stub implementations and cause calls to foo and bar to be dispatched to the NIF implementations instead.

copy from erlang tutorial/nif.html
