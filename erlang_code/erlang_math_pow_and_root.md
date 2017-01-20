#erlang math pow and root
## math pow

```
math:pow(2, 10).
1024.0
math:pow(3, 10).
59049.0
```

## math root

```
trunc(math:log(1024) / math:log(2)).
10
%% from erlang 18
trunc(math:log2(1024)).
10
trunc(math:log(59049) / math:log(3)).
9
round(math:log(59049) / math:log(3)).
10
```
