#' Read IPUMS ACS database table.
#'
#' @param file_db Path of database file to read.
#' @param years Years in data to query (where).
#' @param vars Variables in data to query (select).
#' @return Data frame.
#' @export
db_read_acs <- function(file_db, years, vars) {
  if (!is.numeric(years)) {
    stop("years must be a numeric vector", call. = FALSE)
  }
  if (!is.character(vars)) {
    stop("vars must be a character vector", call. = FALSE)
  }

  vars <- toupper(vars)
  YEAR <- NULL

  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)

  data <- con %>%
    dplyr::tbl("acs") %>%
    dplyr::filter(YEAR %in% years) %>%
    dplyr::select(vars) %>%
    dplyr::collect()

  # fix data types
  if ("INCEARN" %in% vars) {
    data[["INCEARN"]] <- as.integer(data[["INCEARN"]])
  }

  DBI::dbDisconnect(con)
  data
}

#' List variables in IPUMS ACS database table
#'
#' @param file_db Path of database file to read.
#' @return Character vector of variable names from database table.
#' @export
db_list_acs <- function(file_db) {
  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)
  vars <- DBI::dbListFields(con, "acs")
  DBI::dbDisconnect(con)
  vars
}
