# mysql_otp check return info

## update SQL always return ok

``` erlang
%% for example
ok = mysql:query(Pid, "update mytable set id = 42").
```

## check the return info separately

``` erlang
%% Separate calls to fetch more info about the last query
LastInsertId = mysql:insert_id(Pid),
AffectedRows = mysql:affected_rows(Pid),
WarningCount = mysql:warning_count(Pid),
```
copy from the [mysql-otp](https://github.com/mysql-otp/mysql-otp)

## mysql_otp start spec in sys.config

``` erlang
[
  {mysql_poolboy, [
    {pool1,
      {
        [
          {size, 10}, {max_overflow, 20}
        ],
        [
          {host, "localhost"},
          {port, 3306},
          {user, "root"},
          {password, "password"},
          {database, "test"},
          {keep_alive, true},
          {prepare, [{foo, "SELECT sysdate()"}]}
        ]
      }
    }
  ]}
].
```
copy from [MySQL driver for Erlang](http://blog.maxkit.com.tw/2016/06/mysql-driver-for-erlang.html)
More options need to read the mysql.erl source code.
