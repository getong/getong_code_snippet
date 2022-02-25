# edts usage

## .edts example

```
:name "project_name"
:node-name "erlang_node_name"
:lib-dirs '("src" "../lib")
:app-include-dirs '("include" "../lib/include")
:erlang-cookie "random_cookie_string"
```

The `.edts` should be replaced in the erlang project root directory.

## enable compiler debug_info option
The edts requires abstract_code feature, enable the compiler debug_info option.
