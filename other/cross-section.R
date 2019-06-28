# cross section

library(tidyverse)
library(acs)

file_db <- "~/data/acs/acsdb"
years <- 2017

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
  data
}

calc_stats <- function(data, by1, by2, probs = seq(0.1, 0.9, 0.01)) {
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
      p = list(!!probs),
      q = list(rwmisc::wtd_quantile(incwage, perwt, probs = !!probs))
    ) %>%
    group_by(!!!by1_) %>%
    mutate(percent = round(pop / sum(pop) * 100, 1)) %>%
    ungroup()

  out <- left_join(out, dists_tot, by = by2)
  out <- unnest(out)
  out$q[out$n < 100] <- NA
  out
}

plot_stats <- function(data, color) {
  color_ <- sym(color)

  data %>%
    ggplot(aes(p, q, color = !!color_)) +
    geom_point(size = 1.5, alpha = 0.2) +
    geom_smooth(span = 0.5, se = FALSE, size = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), minor_breaks = NULL) +
    scale_y_continuous(limits = c(0, NA), breaks = seq(0, 5e5, 2e4), labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)

res <- data %>%
  filter(sex == "male", age %in% 25:55, incwage > 0) %>%
  calc_stats(by1 = "met2013", by2 = "met2013")

res %>%
  filter(str_detect(met2013, "seattle|harrisburg|las vegas")) %>%
  mutate(met2013 = str_sub(met2013, 1, 20)) %>%
  mutate(met2013 = reorder(met2013, desc(q))) %>%
  plot_stats(color = "met2013")
