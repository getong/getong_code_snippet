#erlang atom

## atom memory

``` erlang
memory(atom).
memory(atom_used).
```

## get all atoms (hacks and no documented)

``` erlang
-module(all_atoms).

-export([all_atoms/0]).

atom_by_number(N) ->
    binary_to_term(<<131,75,N:24>>).

all_atoms() ->
    atoms_starting_at(0).

atoms_starting_at(N) ->
    try atom_by_number(N) of
        Atom ->
            [Atom] ++ atoms_starting_at(N + 1)
    catch
        error:badarg ->
            []
    end.
```

See [Erlang: Can I get a list of all currently-registered atoms?](https://stackoverflow.com/questions/13480462/erlang-can-i-get-a-list-of-all-currently-registered-atoms)

## in 20

``` erlang
erlang:system_info(atom_count).
```

See [Monitoring Erlang Atoms] (https://engineering.klarna.com/monitoring-erlang-atoms-c1d6a741328e)
