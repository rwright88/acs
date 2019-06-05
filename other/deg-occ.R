# degree by occupation stats

library(tidyverse)
library(acs)

file_db <- "~/data/acs/acsdb"
years <- 2017

# funs --------------------------------------------------------------------

get_data <- function(file_db, years) {
  vars <- c("year", "perwt", "sex", "age", "race", "hispan", "educd", "degfield", "occ2010", "incearn")
  data <- acs::acs_db_read(file_db, years = years, vars = vars)
  data <- acs::acs_clean(data)

  cw_occ <- read_csv("data-raw/cw-occupation.csv", col_types = "icc")
  data <- left_join(data, cw_occ, by = c("occ2010" = "occ_code"))
  data <- mutate_at(data, c("occ_cat_name", "occ_name"), str_to_lower)

  data$stem <- rec_stem(data$degfield)
  data$coder <- rec_coder(data$occ_name)
  data
}

rec_stem <- function(x) {
  stem <- c(
    "computer and information sciences",
    "engineering",
    "mathematics and statistics"
  )
  out <- rep("no-degree", length(x))
  out[x %in% stem] <- "stem-degree"
  out[!(x %in% stem) & !is.na(x)] <- "other-degree"
  out
}

rec_coder <- function(x) {
  coder <- c(
    "computer scientists and systems analysts/network systems analysts/web developers",
    "computer programmers",
    "software developers, applications and systems software",
    "database administrators"
  )
  out <- rep(NA_character_, length(x))
  out[x %in% coder] <- "coder"
  out[!(x %in% coder) & !is.na(x)] <- "other"
  out
}

calc_dists <- function(data, by1, by2) {
  by1_ <- syms(by1)
  by2_ <- syms(by2)
  n_years <- length(unique(data[["year"]]))

  dists_tot <- data %>%
    group_by(!!!by2_) %>%
    summarise(pop = sum(perwt) / n_years) %>%
    ungroup() %>%
    mutate(perc_all = round(pop / sum(pop) * 100, 1)) %>%
    select(-pop)

  dists <- data %>%
    group_by(!!!by1_, !!!by2_) %>%
    summarise(
      n = n(),
      pop = sum(perwt) / n_years,
      earn_mean = round(weighted.mean(incearn, perwt), -3),
      earn_p50 = round(Hmisc::wtd.quantile(incearn, perwt, probs = 0.5), -3)
    ) %>%
    group_by(!!!by1_) %>%
    mutate(percent = round(pop / sum(pop) * 100, 1)) %>%
    ungroup() %>%
    left_join(dists_tot, by = by2) %>%
    arrange(!!!by1_, desc(pop))

  dists$earn_mean[dists$n < 100] <- NA_real_
  dists$earn_p50[dists$n < 100] <- NA_real_
  dists
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)

res <- data %>%
  filter(age %in% 26:36) %>%
  calc_dists(
    by1 = c("sex"),
    by2 = c("educd")
  )

res %>%
  arrange(desc(earn_mean))
