# relx
>{release, {sexpr, "0.0.1"},
> [sexpr,
>  %% There are two syntaxes for constraints.
>  %% The first is the tuple syntax shown here.
>  {erlware_commons, "0.8.0", '<='}]}.
>
>{release, {sexpr, "0.0.2"},
> [sexpr,
>
>  %% This is the second constraint syntax, it is interchangeable with the tuple
>  %% syntax and its up to you which you find more readable/usable.
>  "erlware_commons>=0.8.1",
>
>  %% You can put the release load types in the release spec here in exactly the
>  %% same way that you can do it for a normal relfile. The syntax is
>  %% {<constraint>, <type>}.
>  {neotoma, load}]}.

The [relx wiki](https://github.com/erlware/relx/wiki) is a good place to start.


## run_erl and to_erl

``` shell
run_erl /tmp/erl_pipe /tmp/log_dir "erl"
to_erl /tmp/erl_pipe
```

## the relx make the release_handler more robust

The relx also uses the `release_handler`, but it makes the release upgrade opeation more robost
``` erlang
%% we now have the location of the release package, however
%% release handler expects a fixed nomenclature (<relname>.tar.gz)
%% so give it just that by creating a symlink to the tarball
%% we found.
%% make sure that the dir where we're creating the link in exists
ok = filelib:ensure_dir(filename:join([filename:dirname(ReleaseLink), "dummy"])),
%% create the symlink pointing to the full path name of the
%% release package we found
case file:make_symlink(filename:absname(Filename), ReleaseLink) of
    ok ->
     ok;
    {error, eperm} -> % windows!
     {ok,_} = file:copy(filename:absname(Filename), ReleaseLink)
end,
```
The other is the same.
