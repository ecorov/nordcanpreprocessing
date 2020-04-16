




#' @importFrom easyassertions assert_is_data.table_with_required_names
check_nordcan_cancer_case_dataset <- function(
  x,
  check_col_nms = nordcancore::nordcan_col_nms()
) {
  nordcancore::assert_is_set_of_nordcan_col_nms(test_col_nms)
  easyassertions::assert_is_data.table_with_required_names(
    x,
    required_names = check_col_nms
  )

  report <- report_on_nordcan_cancer_case_dataset(
    x = x,
    report_col_nms = check_col_nms
  )
  # TODO: go through the results, raise errors where tests do not pass

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








