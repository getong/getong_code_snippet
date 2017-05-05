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
