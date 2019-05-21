# metros

library(tidyverse)
library(acs)

file_db <- "~/data/acs/acsdb"
years   <- 2017

# funs --------------------------------------------------------------------

get_data <- function(file_db, years) {
  vars <- c("year", "perwt", "met2013", "sex", "age", "race", "hispan", "incearn")
  data <- acs::acs_db_read(file_db, years, vars)
  data <- acs::acs_clean(data)
  data
}

calc_earn <- function(data) {
  n_years <- length(unique(data$year))

  out <- data %>%
    group_by(met2013) %>%
    summarise(
      n = n(),
      pop = sum(perwt) / n_years,
      earn_mean = weighted.mean(incearn, perwt),
      earn_p50 = Hmisc::wtd.quantile(incearn, perwt, probs = 0.5)
    ) %>%
    ungroup()

  out$earn_mean[out$n < 100] <- NA
  out$earn_p50[out$n < 100] <- NA
  out
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)

data %>%
  filter(sex == "male", age %in% 25:54) %>%
  calc_earn() %>%
  arrange(desc(earn_mean))
