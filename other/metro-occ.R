# metro by occupation stats

library(tidyverse)
library(acs)

source("other/ignore/calc.R")

file_db  <- "~/data/acs/acsdb"
file_occ <- "data-raw/cw-occupation.csv"
years    <- 2014:2016
vars     <- c("year", "perwt", "met2013", "sex", "age", "race", "hispan", "educd", "degfield", "occ2010", "incearn")

# funs --------------------------------------------------------------------

clean_acs2 <- function(data, file_occ) {
  cw_occ <- read_csv(file_occ, col_types = "icc")

  data <- data %>%
    left_join(cw_occ, by = c("occ2010" = "occ_code")) %>%
    mutate_at(c("occ_cat_name", "occ_name"), str_to_lower) %>%
    select(year, perwt, met2013, sex, age, race, educd, degfield, occ_cat_name, occ_name, incearn)

  data
}

rec_stem <- function(x) {
  stem <- "engineering$|computer|math|physical sci"
  out <- rep("no-degree", length(x))
  out[str_detect(x, stem)] <- "stem"
  out[!str_detect(x, stem) & !is.na(x)] <- "non-stem"
  out
}

rec_coder <- function(x) {
  coder <- str_c("computer sci|computer prog|software|database")
  out <- rep(NA_character_, length(x))
  out[str_detect(x, coder)] <- "coder"
  out[!str_detect(x, coder) & !is.na(x)] <- "other"
  out
}

# run ---------------------------------------------------------------------

dat <- db_read_acs(file_db, years = years, vars = vars)
dat <- clean_acs(dat)
dat <- clean_acs2(dat, file_occ = file_occ)

dists <- dat %>%
  filter(sex == "male", age %in% 25:54, race %in% c("white", "asian pi")) %>%
  mutate(occ_name = rec_coder(occ_name)) %>%
  calc_dists(
    by1 = c("met2013"),
    by2 = c("occ_name"),
    type = "median"
  )

dists %>%
  filter(occ_name == "coder") %>%
  arrange(desc(percent)) %>%
  .[1:20, ]
