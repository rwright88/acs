#' Read IPUMS ACS database table.
#'
#' @param file_db Path of database file to read.
#' @param years Years in data to query (where).
#' @param vars Variables in data to query (select).
#' @return Data frame.
#' @export
acs_db_read <- function(file_db, years, vars) {
  if (!is.numeric(years)) {
    stop("`years` must be a numeric vector", call. = FALSE)
  }
  if (!is.character(vars)) {
    stop("`vars` must be a character vector", call. = FALSE)
  }

  vars <- toupper(vars)
  YEAR <- NULL

  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)
  on.exit(DBI::dbDisconnect(con))

  data <- dplyr::tbl(src = con, "acs")
  data <- dplyr::filter(data, YEAR %in% years)
  data <- dplyr::select(data, vars)
  data <- dplyr::collect(data)
  data
}

#' List variables in IPUMS ACS database table
#'
#' @param file_db Path of database file to read.
#' @return Character vector of variable names from database table.
#' @export
acs_db_list <- function(file_db) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)
  on.exit(DBI::dbDisconnect(con))
  vars <- DBI::dbListFields(con, "acs")
  vars
}
