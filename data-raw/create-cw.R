# create crosswalk data

library(readr)

cw_degree <- read_csv("data-raw/cw-degree.csv",      col_types = "ic")
cw_metro  <- read_csv("data-raw/cw-metro.csv",       col_types = "ic")
cw_occ    <- read_csv("data-raw/cw-occupation.csv",  col_types = "icc")

cw_occ$occ_name     <- tolower(cw_occ$occ_name)
cw_occ$occ_cat_name <- tolower(cw_occ$occ_cat_name)

usethis::use_data(
  cw_degree,
  cw_metro,
  cw_occ,
  internal = FALSE,
  overwrite = TRUE
)

usethis::use_data(
  cw_degree,
  cw_metro,
  cw_occ,
  internal = TRUE,
  overwrite = TRUE
)
