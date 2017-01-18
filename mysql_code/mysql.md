#mysql
## dump table struct

```
mysqldump --comments -uuser -ppassword -h host_ip --opt database -d --single-transaction | sed 's/ AUTO_INCREMENT=[0-9]*\b//' | sed 's/CREATE TABLE/CREATE TABLE IF NOT EXISTS/g' | sed '/DROP/d' > database_struct.sql
```

## mysql查询结果输出到文件

```
mysql -h host_ip -u user -p database -P 3306 -e "select * from table"  > filename
```
