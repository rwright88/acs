# degree by occupation stats

library(tidyverse)
library(acs)

file_db  <- "~/data/acs/acsdb"
file_occ <- "data-raw/cw-occupation.csv"
years    <- 2015:2017
vars     <- c("year", "perwt", "sex", "age", "race", "hispan", "educd", "degfield", "occ2010", "incearn")

# funs --------------------------------------------------------------------

acs_clean2 <- function(data, file_occ) {
  cw_occ <- read_csv(file_occ, col_types = "icc")
  data <- left_join(data, cw_occ, by = c("occ2010" = "occ_code"))
  data <- mutate_at(data, c("occ_cat_name", "occ_name"), str_to_lower)
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

  dists$earn_mean[dists$earn_mean < 100] <- NA_real_
  dists$earn_p50[dists$earn_p50 < 100] <- NA_real_
  dists
}

# run ---------------------------------------------------------------------

dat <- acs_db_read(file_db, years = years, vars = vars)
dat <- acs_clean(dat)
dat <- acs_clean2(dat, file_occ = file_occ)
dat$stem <- rec_stem(dat$degfield)
dat$coder <- rec_coder(dat$occ_name)

res <- dat %>%
  filter(age %in% 26:36) %>%
  calc_dists(
    by1 = c("sex", "race"),
    by2 = c("coder", "stem")
  )

res %>%
  filter(sex == "male", race == "white") %>%
  arrange(desc(earn_p50))
