# degree by occupation stats

library(tidyverse)
library(acs)

source("other/ignore/calc.R")

file_db  <- "~/data/acs/acsdb"
file_occ <- "data-raw/cw-occupation.csv"
years    <- 2014:2016
vars     <- c("year", "perwt", "met2013", "sex", "age", "race", "hispan", "educd", "degfield", "occ2010", "incearn")

# funs --------------------------------------------------------------------

acs_clean2 <- function(data, file_occ) {
  cw_occ <- read_csv(file_occ, col_types = "icc")

  data <- data %>%
    left_join(cw_occ, by = c("occ2010" = "occ_code")) %>%
    mutate_at(c("occ_cat_name", "occ_name"), str_to_lower) %>%
    select(year, perwt, met2013, sex, race, age, educd, degfield, occ_cat_name, occ_name, incearn)

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

plot_age <- function(dists, by) {
  by <- sym(by)

  dists %>%
    mutate(!!by := reorder(!!by, desc(earn_stat))) %>%
    ggplot(aes(age, earn_stat, color = !!by)) +
    geom_point(size = 2) +
    geom_smooth(span = 1, se = FALSE, size = 0.5) +
    scale_y_continuous(limits = c(0, NA), breaks = seq(0, 1e6, 2e4), labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()
}

# run ---------------------------------------------------------------------

dat <- acs_db_read(file_db, years = years, vars = vars)
dat <- acs_clean(dat)
dat <- acs_clean2(dat, file_occ = file_occ)

dists <- dat %>%
  filter(sex == "male", age %in% 25:54) %>%
  mutate(occ_name = rec_coder(occ_name)) %>%
  calc_dists(
    by1 = c("age"),
    by2 = c("occ_name"),
    type = "median"
  )

dists %>%
  filter(occ_name == "coder") %>%
  ggplot(aes(age, percent)) +
  geom_point() +
  geom_line() +
  scale_y_continuous(limits = c(0, NA)) +
  theme_bw()
