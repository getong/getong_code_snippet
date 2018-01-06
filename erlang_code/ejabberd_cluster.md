# ejabberd cluster
ejabberd first get the `node_id` by `erlang:phash2` function, and then get the hash back to the node.

``` erlang
-spec node_id() -> binary().
node_id() ->
    integer_to_binary(erlang:phash2(node())).

-spec get_node_by_id(binary()) -> node().
get_node_by_id(Hash) ->
    try binary_to_integer(Hash) of
	I -> match_node_id(I)
    catch _:_ ->
	    node()
    end.

-spec match_node_id(integer()) -> node().
match_node_id(I) ->
    match_node_id(I, get_nodes()).

-spec match_node_id(integer(), [node()]) -> node().
match_node_id(I, [Node|Nodes]) ->
    case erlang:phash2(Node) of
	I -> Node;
	_ -> match_node_id(I, Nodes)
    end;
match_node_id(_I, []) ->
    node().
```
