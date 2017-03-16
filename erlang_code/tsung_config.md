#tsung config

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
