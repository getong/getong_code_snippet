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

## with query
see [Common Table Expressions](https://www.postgresql.org/docs/current/queries-with.html)
see [Get to Know the Power of SQL Recursive Queries](https://academy.vertabelo.com/blog/get-to-know-the-power-of-sql-recursive-queries/)
see [Do It in SQL: Recursive Tree Traversal](https://academy.vertabelo.com/blog/do-it-in-sql-recursive-tree-traversal/)
see [Ecto preload for tag has_many tags](https://elixirforum.com/t/ecto-preload-for-tag-has-many-tags/4323)
see [Closure Table – Store Hierarchical Data Seamlessly | PostgreSQL](https://www.technobytz.com/closure_table_store_hierarchical_data.html)

## check a table exist

``` sql
SELECT EXISTS (
   SELECT 1
   FROM   information_schema.tables
   WHERE  table_schema = 'schema_name'
   AND    table_name = 'table_name'
   );
```
copy from [How to check if a table exists in a given schema](https://stackoverflow.com/questions/20582500/how-to-check-if-a-table-exists-in-a-given-schema)

## alter sequence

``` sql
ALTER SEQUENCE product_id_seq RESTART WITH 1453
```
`product` is the table name
copy from [Reset auto increment counter in postgres](https://stackoverflow.com/questions/5342440/reset-auto-increment-counter-in-postgres)

## ERROR: column c.relhasoids does not exist
the client and server version should be the same, the 12 version.

``` shell
docker exec -it postgres_container_name psql your_connection_string
```
copy from [How to fix “ERROR: column c.relhasoids does not exist” in Postgres?](https://stackoverflow.com/questions/58461178/how-to-fix-error-column-c-relhasoids-does-not-exist-in-postgres)

## change schema

``` sql
set search_path to schema1, schema2;
```
