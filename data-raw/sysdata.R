
reports_on_column_name_sets <- data.table::fread(
  "data-raw/reports_on_column_sets.csv"
)
data.table::setkeyv(reports_on_column_name_sets, "report_fun_nm")

usethis::use_data(
  reports_on_column_name_sets, overwrite = TRUE, internal = TRUE
)
