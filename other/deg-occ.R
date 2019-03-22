# degree by occupation stats

library(tidyverse)
library(acs)

source("other/calc.R")

file_db  <- "~/data/acs/acsdb"
file_occ <- "data-raw/cw-occupation.csv"
years    <- 2014:2016
vars     <- c("year", "perwt", "sex", "age", "race", "hispan", "educd", "degfield", "occ2010", "incearn")

# funs --------------------------------------------------------------------

acs_clean2 <- function(data, file_occ) {
  cw_occ <- read_csv(file_occ, col_types = "icc")

  data <- data %>%
    left_join(cw_occ, by = c("occ2010" = "occ_code")) %>%
    mutate_at(c("occ_cat_name", "occ_name"), str_to_lower)

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

dat <- acs_db_read(file_db, years = years, vars = vars)
dat <- acs_clean(dat)
# dat <- acs_clean2(dat, file_occ = file_occ)

rwmisc::summary2(dat)

dists <- dat %>%
  filter(race == "white", age %in% 30:49) %>%
  calc_dists(
    by1 = c("sex"),
    by2 = c("educd", "degfield"),
    type = "median"
  )

dists %>%
  filter(sex == "male", educd != "advanced") %>%
  arrange(desc(earn_stat))
