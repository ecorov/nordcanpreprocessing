


# vocabulary:
# - test function: returns TRUE / FALSE values
# - check: always returns NULL invisibly, raises error if tests do not pass
# - assertion: a check designed for validating function arguments;
#   i.e. a check function's arguments other than the main one might be asserted
#   (e.g. in check_my_data(my_data, something = "something") argument something
#   might be asserted)
# - report: runs tests and returns a human-readable explanation of what was not
#   right; i.e. a check function might run a report function and raise an error
#   using the contents of the report

#' @importFrom nordcancore nordcan_categorical_col_nms
#' nordcan_column_level_space nordcan_date_col_nms
#' @importFrom data.table data.table
report_on_column <- function(
  values,
  col_nm
) {
  nordcancore::assert_is_set_of_nordcan_col_nms(col_nm)

  report_dt <- data.table::data.table(
    test = character(0L),
    passed = logical(0L),
    report = character(0L)
  )

  if (col_nm %in% nordcancore::nordcan_categorical_col_nms()) {
    # are observed values sensible?
    col_level_space <- nordcancore::nordcan_column_level_space(col_nm)

    extra_levels <- setdiff(values, col_level_space)
    has_extra_levels <- length(extra_levels) > 0
    missing_levels <- setdiff(col_level_space, values)
    has_missing_levels <- length(missing_levels) > 0

    report_dt <- rbind(
      report_dt,
      data.table::data.table(
        test = c("has_extra_levels", "has_missing_levels"),
        passed = c(has_extra_levels, has_missing_levels),
        # TODO: better reports
        report = c(deparse(extra_levels), deparse(missing_levels))
      )
    )

  } else if (col_nm %om% nordcancore::nordcan_date_col_nms()) {
    # is date sensible?
  } else {
    # something else
  }

  if (col_nm == "DoB") {
    # is date of birth sensible? for instance something like this:
    report_dt <- rbind(
      report_dt,
      data.table::data.table(
        test = "DoB_gt_1800-01-01",
        passed = all(values > as.Date("1800-01-01")),
        report = paste0(
          sum(values <= as.Date("1800-01-01")),
          " DoB values were <= 1800-01-01"
        )
      )
    )
  }

  report <- list(
    all_tests_passed = all(report_dt[["passed"]]),
    report_dt = report_dt
  )
  return(report)
}




#' @importFrom nordcancore nordcan_col_nms
#' @importFrom easyassertions assert_is_data.table_with_required_names
report_on_nordcan_cancer_dataset <- function(
  x,
  report_col_nms = NULL
) {
  if (is.null(report_col_nms)) {
    report_col_nms <- nordcancore::nordcan_col_nms()
  }
  nordcancore::assert_is_set_of_nordcan_col_nms(report_col_nms)
  easyassertions::assert_is_data.table_with_required_names(
    x,
    required_names = report_col_nms
  )

  # reports on individual columns
  single_column_reports <- lapply(report_col_nms, function(col_nm) {
    report_on_column(values = x[[col_nm]], col_nm = col_nm)
  })
  names(single_column_reports) <- report_col_nms

  # brought together
  report_dt <- data.table::rbindlist(lapply(report_col_nms, function(col_nm) {
    report_dt <- single_column_reports[[col_nm]][["report_dt"]]
    cbind(col_nm = col_nm, report_dt)
  }))

  # reports on specific combinations of columns; here's a hypothetical example
  reports_on_column_sets <- nordcancore::get_internal_dataset(
    dataset_name = "reports_on_column_sets",
    package_name = "nordcanpreprocessing"
  )

  # TODO:
  # - infer which column set report functions to call by parsing info in
  #   reports_on_column_sets
  # - call those report functions
  # - collect all results together into report_dt

  return(report_dt)
}




report_on_bi_date_dg_date <- function(x) {
  easyassertions::assert_is_data.table_with_required_names(
    x,
    c("bi_date", "dg_date")
  )
  # TODO
}




