# benchmark creating database, reading data, and cleaning data

library(acs)

file_data <- "~/data/acs/acs.csv"
file_db   <- "~/data/acs/acsdb"

years_read <- c(1980, 1990, 2000, 2017)
vars_read <- c("year", "perwt")

# 2019-02-09: ~ 3GB / 280s
system.time({
  db_write_acs(
    file_data = file_data,
    file_db = file_db
  )
})

# 2019-02-09: ~ 8s
system.time({
  dat <- db_read_acs(
    file_db = file_db,
    years = years_read,
    vars = vars_read
  )
})

# 2019-02-09: ~ 0.1s
system.time({
  dat <- clean_acs(dat)
})

dplyr::summarise(dplyr::group_by(dat, year), pop = sum(perwt))
