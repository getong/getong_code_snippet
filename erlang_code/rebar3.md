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
RELX_CONFIG_PATH="sys.config" VMARGS_PATH="vm.args" ./erl_proj console
```

## rebar3 shell in hidden mode

``` shell
rebar3 local install
export PATH=/home/user/.cache/rebar3/bin:$PATH
REBAR3_ERL_ARGS="-hidden" rebar3 shell --name=a@hostname --setcookie cookie
```
see [Would rebar3 shell add hidden option? #1565](https://github.com/erlang/rebar3/issues/1565)


## exclude some beam
See [Build fails with "Duplicated modules" error ](https://github.com/erlware/relx/issues/463)
rabbitmq-erlang-client use some module from mochiweb with the same name, so they are `Duplicated modules`.
The rabbitmq-erlang-client has change the module in the master branch, but not released in 3.6.*, and will release in 3.7.
So now use the exclude directive.
```
{exclude_modules, [{App :: atom(), [Module :: atom()]}]}
```

## edoc option
copy from [edoc: error in doclet 'edoc_doclet' when using macro in -type](http://erlang.org/pipermail/erlang-questions/2015-October/086562.html)
add below into rebar.config
```
{edoc_opts, [
    {includes,["deps/gpb/include"]},
    {preprocess, true}
    ]}.
```
## rebar3 use proxy

``` shell
export http_proxy=http://127.0.0.1:8118
export https_proxy=http://127.0.0.1:8118
rebar3 update
rebar3 upgrade
```
The 8118 is set up by privoxy.

## push library to hex.pm
copy from [Publishing a package](https://github.com/tsloughter/rebar3_hex)
Using the [rebar3_hex](https://github.com/tsloughter/rebar3_hex)
``` shell
# add this to ~/.config/rebar3/rebar.config
echo "{plugins, [rebar3_hex]}." >> ~/.config/rebar3/rebar.config

# register the hex.pm
rebar3 hex user register

## login the hex.pm
rebar3 hex user auth

## under the project, fix the code and version info

## push
 rebar3 hex publish

```
The hex commands:

``` shell
hex config <key> [<value>]
hex cut [-i major|minor|patch]
hex docs
hex info [<package> [<version>]]
hex key [remove key_name|list]
hex publish
hex owners [add <package> <email>|remove <package> <email>|list <package>]
hex user [register|whoami|auth|deauth|reset_password]
hex search <term>
```
copy from [rebar3_hex](https://github.com/tsloughter/rebar3_hex)

## releases
see the manual [Elixir: how can I leverage release_handler?](https://stackoverflow.com/questions/36196148/elixir-how-can-i-leverage-release-handler)

## rebar3_gpb_plugin
if the project is a `app` rebar3 project type, just add all the options into the rebar.config.
But if the project is `release`, another rebar3 project type, just move all the option into the `apps/project_name/rebar.config`, only add this plugin into the rebar.config right under the project. See the examples in the plugin, they demonstrates it.
