# rebar3

## rebar3 release usage

``` shell
# new a release project
rebar3 new release erl_project

cd release_project
# compile the project
rebar3 compile

# tar the project
rebar3 as prod tar

# make the release into other dir
rebar3 as prod release -o other_dir
cd other_dir

# change the config file
ERLX_CONFIG_PATH="sys.config" VMARGS_PATH="vm.args" ./erl_proj console
```

## rebar3 shell in hidden mode

``` shell
rebar3 local install
export PATH=/home/user/.cache/rebar3/bin:$PATH
REBAR3_ERL_ARGS="-hidden" rebar3 shell --name=a@hostname --setcookie cookie
```
see [Would rebar3 shell add hidden option? #1565](https://github.com/erlang/rebar3/issues/1565)
