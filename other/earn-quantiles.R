# earnings quantiles

library(tidyverse)
library(acs)

source("other/ignore/calc.R")

file_db <- "~/data/acs/acsdb"
years   <- 2014:2016
vars    <- c("year", "perwt", "sex", "age", "degfield", "incearn")

# run ---------------------------------------------------------------------

dat <- acs_db_read(file_db, years = years, vars = vars)
dat <- acs_clean(dat)

quantiles <- dat %>%
  filter(sex == "male", age %in% 30:49) %>%
  calc_q_earn(by = c("degfield"), probs = c(0.25, 0.5, 0.75))

quantiles %>%
  arrange(desc(p_50))
