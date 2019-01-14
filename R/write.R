#' Write database table from CSV file
#'
#' This will overwrite the existing database table with the same name.
#'
#' @param file_data Path of CSV file of IPUMS ACS data.
#' @param file_db Path of database to create.
#' @export
write_db_table <- function(file_data, file_db) {
  # assume every variable from IPUMS data is an integer
  n_cols <- length(read.csv(file_data, nrows = 1))
  types  <- rep("integer", n_cols)

  # names1 <- names(read.csv(file_data, nrows = 1))
  # n_cols <- length(names1)
  # types  <- rep("integer", n_cols)
  # names(types) <- names1

  con <- DBI::dbConnect(RSQLite::SQLite(), file_db)

  DBI::dbWriteTable(
    conn = con,
    name = "acs",
    value = file_data,
    overwrite = TRUE,
    # field.types = types,
    colClasses = types,
    sep = ","
  )

  DBI::dbExecute(con, statement = "CREATE INDEX idx1 ON acs(YEAR)")
  DBI::dbDisconnect(con)
}
