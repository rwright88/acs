# cross section

library(tidyverse)
library(acs)

file_db <- "~/data/acs/acsdb"
years <- 2015:2017

# funs --------------------------------------------------------------------

get_data <- function(file_db, years) {
  vars <- c(
    "year", "perwt", "met2013", "sex", "age", "race", "hispan",
    "educd", "degfield", "occ2010", "uhrswork", "incwage"
  )
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

calc_stats <- function(data, by1, by2, probs = c(0.25, 0.5, 0.75)) {
  by1_ <- syms(by1)
  by2_ <- syms(by2)
  n_years <- length(unique(data$year))

  dists_tot <- data %>%
    group_by(!!!by2_) %>%
    summarise(pop = sum(perwt) / n_years) %>%
    ungroup() %>%
    mutate(perc_all = round(pop / sum(pop) * 100, 1)) %>%
    select(-pop)

  out <- data %>%
    group_by(!!!by1_, !!!by2_) %>%
    summarise(
      n = n(),
      pop = sum(perwt) / !!n_years,
      p = list(paste0("p_", !!probs * 100)),
      q = list(round(Hmisc::wtd.quantile(incwage, perwt, probs = !!probs), -3))
    ) %>%
    group_by(!!!by1_) %>%
    mutate(percent = round(pop / sum(pop) * 100, 1)) %>%
    ungroup()

  out <- left_join(out, dists_tot, by = by2)
  out <- unnest(out)
  out$q[out$n < 100] <- NA
  out <- spread(out, p, q)
  out
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)

res <- data %>%
  filter(sex == "male", age %in% 25:55, incwage > 0) %>%
  calc_stats(by1 = "met2013", by2 = "met2013")

res %>%
  filter(str_detect(met2013, ", pa|, nv|, va")) %>%
  arrange(desc(p_50)) %>%
  print(n = Inf)
