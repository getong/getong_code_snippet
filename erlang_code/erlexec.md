#erlexec

## erl shell file
In the `erl` file, it is just a shell file.

``` shell
ROOTDIR="/usr/lib64/erlang"
BINDIR=$ROOTDIR/erts-8.3/bin
EMU=beam
PROGNAME=`echo $0 | sed 's/.*\///'`
export EMU
export ROOTDIR
export BINDIR
export PROGNAME
exec "$BINDIR/erlexec" ${1+"$@"}
```
It just call the `erlexec` command.

## rebar3 release script

``` shell
        set -- "$BINDIR/erlexec" $FOREGROUNDOPTIONS \
            -boot "$BOOTFILE" -mode "$CODE_LOADING_MODE" \
            -boot_var ERTS_LIB_DIR "$ERTS_LIB_DIR" \
            -config "$RELX_CONFIG_PATH" \
            -args_file "$VMARGS_PATH" \
            -pa ${__code_paths}

```
The target release also uses `erlexec`, the `erlexec` is obvious the erlang release beginning start.

## what is erlexec

``` shell
$ file erlexec
erlexec: ELF 64-bit LSB executable, x86-64, version 1 (SYSV), dynamically linked, interpreter /lib64/ld-linux-x86-64.so.2, for GNU/Linux 2.6.32, BuildID[sha1]=b15c57f3fe1229320ce4e4a44356e18914adcb0e, not stripped
```
