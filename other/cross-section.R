# cross section

library(acs)
library(dplyr)
library(ggplot2)
library(readr)
library(rwmisc)
library(tidyr)

file_db <- "~/data/acs/acsdb"
years <- 2017

# funs --------------------------------------------------------------------

get_data <- function(file_db, years) {
  vars <- c(
    "year", "perwt", "met2013", "sex", "age", "race", "hispan", "educd",
    "degfield", "occ2010", "wkswork2", "uhrswork", "incwage"
  )
  data <- acs::acs_db_read(file_db, years = years, vars = vars)
  data <- acs::acs_clean(data)
  cw_occ <- read_csv("data-raw/cw-occupation.csv", col_types = "icc")
  data <- left_join(data, cw_occ, by = c("occ2010" = "occ_code"))
  data <- mutate_at(data, c("occ_cat_name", "occ_name"), tolower)
  data
}

calc_stats <- function(data, by) {
  by_ <- syms(by)
  probs <- seq(0.1, 0.9, 0.01)

  out <- data %>%
    group_by(!!!by_) %>%
    summarise(
      n = n(),
      pop = sum(perwt),
      p = list(!!probs),
      q = list(rwmisc::wtd_quantile(incwage, perwt, probs = !!probs))
    ) %>%
    unnest() %>%
    ungroup()

  out$q[out$n < 100] <- NA
  out
}

plot_latest <- function(data, color = NULL) {
  if (!is.null(color)) {
    color_ <- sym(color)
    out <- ggplot(data, aes(p, q, color = !!color_))
  } else {
    out <- ggplot(data, aes(p, q))
  }

  out +
    geom_point(size = 1.5, alpha = 0.2) +
    geom_smooth(span = 0.5, se = FALSE, size = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), minor_breaks = NULL) +
    scale_y_log10(breaks = seq(2e4, 2e5, 2e4), minor_breaks = NULL, labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)

res <- data %>%
  filter(sex == "male", age %in% 25:55, incwage > 0) %>%
  calc_stats(by = "met2013")

res %>%
  filter(grepl("seattle|harrisburg|las vegas", met2013)) %>%
  mutate(met2013 = substr(met2013, 1, 15)) %>%
  mutate(met2013 = reorder(met2013, desc(q))) %>%
  plot_latest(color = "met2013")
