# create crosswalk data

library(readr)

cw_degree <- read_csv("data-raw/cw-degree.csv", col_types = "ic")
cw_metro  <- read_csv("data-raw/cw-metro.csv",  col_types = "ic")

usethis::use_data(
  cw_degree,
  cw_metro,
  internal = FALSE,
  overwrite = TRUE
)

usethis::use_data(
  cw_degree,
  cw_metro,
  internal = TRUE,
  overwrite = TRUE
)
