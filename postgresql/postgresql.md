# postgresql sql command

## describe

``` sql
SELECT
   COLUMN_NAME
FROM
   information_schema.COLUMNS
WHERE
   TABLE_NAME = 'city';
```
copy from [PostgreSQL Describe Table](http://www.postgresqltutorial.com/postgresql-describe-table/)
