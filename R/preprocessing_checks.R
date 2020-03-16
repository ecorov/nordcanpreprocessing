




check_nordcan_cancer_case_dataset <- function(x) {
  check_col_nms <- c("sex", "region")
  nordcancore:::assert_is_data_table_with_required_names(
    x,
    required_names = check_col_nms
  )

  check_funs <- mget(paste0("check_", check_col_nms))
  lapply(check_col_nms, function(col_nm) {
    check_funs[[col_nm]](x[[col_nm]])
  })

  invisible(NULL)
}


check_categorical_column <- function(values, col_nm = "sex") {
  expected_levels <- nordcancore::get_column_level_space(col_nm)[[1L]]

  observed_levels <- sort(unique(values))
  extra_levels <- setdiff(observed_levels, expected_levels)

  missing_levels <- setdiff(expected_levels, observed_levels)
  # raise appropriate errors

  invisible(NULL)
}

check_sex <- function(values) {
  check_categorical_column(values, col_nm = "sex")
}
check_region <- function(values) {
  check_categorical_column(values, col_nm = "region")
}
check_nuts <- function(values) {
  check_categorical_column(values, col_nm = "nuts")
}
check_agegroup <- function(values) {
  check_categorical_column(values, col_nm = "agegroup")
}

check_year <- function(values) {
  # raise appropriate errors
  invisible(NULL)
}
# etc.








