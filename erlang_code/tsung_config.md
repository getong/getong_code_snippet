# tsung config

## read from xml config file
tsung read the config file using xml
```
read(Filename=standard_io, LogDir) ->
    ?LOG("Reading config file from stdin~n", ?NOTICE),
    XML = read_stdio(),
    handle_read(catch xmerl_scan:string(XML,
                                      [{fetch_path,["/usr/share/tsung/","./"]},
                                       {validation,true}]),Filename,LogDir);
read(Filename, LogDir) ->
    ?LOGF("Reading config file: ~s~n", [Filename], ?NOTICE),
    Result = handle_read(catch xmerl_scan:file(Filename,
                                               [{fetch_path,["/usr/share/tsung/","./"]},
                                                {validation,true}]),Filename,LogDir),
    %% In case of error we reparse the file with xmerl_sax_parser:file/2 to obtain
    %% a more verbose output
    case Result of
        {ok, _} -> Result;
        _ -> xmerl_sax_parser:file(Filename,[]),
             Result
    end.
```
copy from ts_config.erl


## get ip or hostname string to erlang ip

```
%% Toresolve is ip string or hostname string
inet:getaddr(ToResolve,inet).
inet:getaddr(ToResolve,inet6).
```
copy from ts_config.erl

## tsung support scan from linux eth0 or similar network interface

```
get_intf_aliases(Interface) ->
    case file:read_file_info("/sbin/ip") of
        {ok,_} ->
            Res=os:cmd("LC_ALL=C /sbin/ip -o -f inet addr show dev "++Interface),
            get_ip_aliases(string:tokens(Res,"\n"), []);
        {error,Reason} ->
            ?LOGF("ip command not found (~p), using ifconfig instead~n",[Reason],?NOTICE),
            Res=os:cmd("LC_ALL=C /sbin/ifconfig "),
            get_intf_aliases(string:tokens(Res,"\n"), Interface,[],[])
    end.


get_ip_aliases([], Res) ->
    Res;
get_ip_aliases([Line|Tail], Res) ->
    [_,_,_,Net|_] =string:tokens(Line," "),
    [TmpIP|_] =string:tokens(Net,"/"),
    ?LOGF("found IP: ~p~n",[TmpIP],?DEB),
    {ok, IP } = inet:getaddr(TmpIP,inet),
    get_ip_aliases(Tail,  [IP|Res]).


get_intf_aliases([], _, _, Res) ->
    Res;
get_intf_aliases(["          inet addr:"++Line|Tail], Interface, Interface, Res) ->
    [TmpIP|_] =string:tokens(Line," "),
    ?LOGF("found IP: ~p~n",[TmpIP],?DEB),
    {ok, IP } = inet:getaddr(TmpIP,inet),
    get_intf_aliases(Tail, Interface, Interface, lists:append([IP],Res));
get_intf_aliases(["          "++_Line|Tail], Interface, Current, Res) ->
    get_intf_aliases(Tail, Interface, Current, Res);
get_intf_aliases([" "|Tail], Interface, Old, Res) ->
    get_intf_aliases(Tail, Interface, Old, Res);
get_intf_aliases([Line|Tail], Interface, Old, Res) ->
    ?LOGF("scan line : ~p~n",[Line],?DEB),
    %% ?DebugF("scan line : ~p~n",[Line]),
    case string:str(Line,Interface) of
        1 ->
            [Current|_] =string:tokens(Line," "),
            ?LOGF("found interface (old is ~p): ~p~n",[Old,Current],?DEB),
            case string:str(Current, Old++":") of
                1 -> % subinterface, don't change current
                    get_intf_aliases(Tail, Interface, Old, Res);
                _ ->
                    get_intf_aliases(Tail, Interface, Current, Res)
            end;
        _ ->
            get_intf_aliases(Tail, Interface, "", Res)
    end.
```
copy from ts_ip_scan.erl

tsung get the ip from the cmd line output:

```
$LC_ALL=C /sbin/ip -o -f inet addr show dev eth0
2: eth0    inet xxx.xxx.xxx.xxx/24 brd xxx.xxx.xxx.xxx scope global dynamic eth0\       valid_lft 58180sec preferred_lft 58180sec
```
It only outputs the ipv4 ip address.
Does all ipv6 comes with a ipv4 ip address?
Gets all the network interface address with:

```
ip a
```
The `ifconfig` does the same work.


## re for dynamic var

```
get_dynvar_name(VarNameStr) ->
    %% check if the var name is for an array (myvar[N])
    case re:run(VarNameStr,"(.+)\[(\d+)\]",[{capture,all_but_first,list},dotall]) of
        {match,[Name,Index]} -> {list_to_atom(Name),Index};
        _                    -> list_to_atom(VarNameStr)
    end.
```

## Plugin parse_config

```
%%% Parsing the request element
parse(Element = #xmlElement{name=request, attributes=Attrs},
      Conf = #config{sessions=[CurSess|_], curid=Id}) ->

    Type  = CurSess#session.type,
    SubstitutionFlag  = getAttr(atom, Attrs, subst, false),
    Tag = getAttr(string, Attrs, tag, ""),
    Tags = lists:map(fun(X)->{X,ok} end, string:tokens(?config(exclude_tag),",")),
    %% do not add in Conf excluded requests
    case proplists:is_defined(Tag, Tags) of
        true ->
            ?LOGF("Tag  ~p in ~p ~p ~p ~n",[Tag,true,?config(exclude_tag),Tags],?NOTICE),
            Conf;
        false ->
            lists:foldl( fun(A,B) ->Type:parse_config(A,B) end,
                         Conf#config{curid=Id+1, cur_req_id=Id+1,
                                     subst=SubstitutionFlag,
                                     match=[],
                                     tag=Tag
                                    },
                         Element#xmlElement.content)
        end;
```
The mysql, ldap, http are all added the `parse_config` option in a module.
