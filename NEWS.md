# scaledescr 0.2.2

## Improvements
* `make_dataframe_to_output()`: now trims whitespace and standardizes case in character variables so that values such as "student", " Student ", and "STUDENT" are treated as the same category.


## New Export Functions
* `make_demographic_table_to_output()` - Generates a demographic summary table using `make_demographic_table()` and exports the resulting table to Word, Excel, or CSV format.
