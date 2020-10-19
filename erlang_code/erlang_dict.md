# erlang dict

## definition

``` erlang
%% Note: mk_seg/1 must be changed too if seg_size is changed.
-define(seg_size, 16).
-define(max_seg, 32).
-define(expand_load, 5).
-define(contract_load, 3).
-define(exp_size, (?seg_size * ?expand_load)).
-define(con_size, (?seg_size * ?contract_load)).

-type segs(_Key, _Value) :: tuple().

%% Define a hashtable.  The default values are the standard ones.
-record(dict,
	{size=0		      :: non_neg_integer(),   	% Number of elements
	 n=?seg_size	      :: non_neg_integer(),   	% Number of active slots
	 maxn=?seg_size	      :: non_neg_integer(),	% Maximum slots
	 bso=?seg_size div 2  :: non_neg_integer(),   	% Buddy slot offset
	 exp_size=?exp_size   :: non_neg_integer(),   	% Size to expand at
	 con_size=?con_size   :: non_neg_integer(),   	% Size to contract at
	 empty		      :: tuple(),		% Empty segment
	 segs		      :: segs(_, _)	      	% Segments
	}).
```


## code reading
[erlang的dict源码解析](https://codeleading.com/article/89602511237/)
