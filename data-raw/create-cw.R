# create crosswalk data

library(readr)

cw_metro <- read_csv("data-raw/cw-metro.csv", col_types = "ic")

usethis::use_data(
  cw_metro,
  internal = FALSE,
  overwrite = TRUE
)

usethis::use_data(
  cw_metro,
  internal = TRUE,
  overwrite = TRUE
)
