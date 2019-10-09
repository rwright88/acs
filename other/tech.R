# tech

library(acs)
library(dplyr)
library(ggplot2)
library(rwmisc)

file_db <- "~/data/acs/acsdb"
years <- 2015:2017
wage_fix <- 1.1

# funs --------------------------------------------------------------------

get_data <- function(file_db, years) {
  vars <- c(
    "year", "perwt", "met2013", "sex", "age", "race", "hispan", "educd",
    "degfield", "occ2010", "incwage"
  )
  data <- acs::acs_db_read(file_db, years = years, vars = vars)
  data <- acs::acs_clean(data)
  left_join(data, acs::cw_occ, by = c("occ2010" = "occ_code"))
}

calc_stats <- function(data, by1, by2) {
  out <- group_by(data, !!!syms(by1))
  out <- summarise(out, n = n(), pop = sum(perwt))
  out <- group_by(out, !!!syms(by2))
  out <- mutate(out, percent = round(pop / sum(pop) * 100, 1))
  out <- ungroup(out)

  p <- seq(0.4, 0.6, 0.01)
  wage <- data[which(data$incwage > 0), ]
  wage <- group_by(wage, !!!syms(by1))
  wage <- summarise(wage,
    wage_p50 = round(mean(rwmisc::wtd_quantile(incwage, perwt, probs = !!p)), -3)
  )
  wage <- ungroup(wage)

  out <- left_join(out, wage, by = by1)
  out$wage_p50[out$n < 100] <- NA
  out
}

rec_tech <- function(x) {
  tech <- "computer|engineering$|math"
  out <- rep(NA_character_, length(x))
  out[grepl(tech, x)] <- "tech"
  out[!grepl(tech, x)] <- "non-tech"
  out[is.na(x)] <- "no-degree"
  out
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)
data$incwage <- data$incwage * wage_fix

data %>%
  filter(race != "other", age %in% 25:35) %>%
  mutate(tech = rec_tech(degfield)) %>%
  calc_stats(by1 = c("sex", "race", "tech"), by2 = c("sex", "race")) %>%
  arrange(desc(wage_p50)) %>%
  print(n = Inf)
