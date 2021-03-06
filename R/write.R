#' Write IPUMS ACS database table
#'
#' Write database table from IPUMS ACS CSV file. This will overwrite the
#' existing database file with the same name.
#'
#' @param file_data Path of CSV file of IPUMS ACS data.
#' @param file_db Path of database to create.
#' @export
acs_db_write <- function(file_data, file_db) {
  stopifnot(file.exists(file_data))
  nms <- names(utils::read.csv(file_data, nrows = 1))
  n_cols <- length(nms)
  types  <- rep("integer", n_cols)
  names(types) <- nms

  if (file.exists(file_db)) {
    file.remove(file_db)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbWriteTable(
    conn = con,
    name = "acs",
    value = file_data,
    overwrite = TRUE,
    field.types = types,
    sep = ","
  )

  DBI::dbExecute(con, statement = "CREATE INDEX idx1 ON acs(YEAR)")
}
