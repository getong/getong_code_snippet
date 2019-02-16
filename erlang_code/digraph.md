#erlang digraph operation

``` erlang
1> Graph = digraph:new().
{digraph,#Ref<0.2174343961.794427395.117958>,
         #Ref<0.2174343961.794427395.117959>,
                  #Ref<0.2174343961.794427395.117960>,true}
2> digraph:add_vertex(Graph, a).
a
3> digraph:add_vertex(Graph, b).
b
4> digraph:add_edge(Graph, a, b).
['$e'|0]
5> digraph:get_short_path(Graph, a, b).
[a,b]
```
