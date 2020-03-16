




enrich_nordcan_cancer_case_dataset <- function(x) {
  x <- data.table::copy(x)

  # create new columns, drop any unnecessary ones, etc.
  x[, "MoB" := data.table::month(birth_date)]
}





