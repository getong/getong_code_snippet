# xlsxir usage
xlsxir is a elixir library for excel

## use example

``` elixir
sheet_id_list = Xlsxir.multi_extract(path)
```
every sheet in the excel file will be open in a table id.
and then it can be used in the functions below:

``` elixir
Xlsxir.get_list(table_id)
Xlsxir.get_map(table_id)
Xlsxir.get_mda(table_id)
Xlsxir.get_cell(table_id, cell_ref)
Xlsxir.get_row(table_id, row_num)
Xlsxir.get_col(table_id, col_ltr)
Xlsxir.get_info(table_id, num_type)
```
