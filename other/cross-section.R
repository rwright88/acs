# cross section

library(acs)
library(dplyr)
library(ggplot2)
library(rwmisc)
library(tidyr)

file_db <- "~/data/acs/acsdb"
years <- 2015:2017
wage_fix <- 1.1

# funs --------------------------------------------------------------------

get_data <- function(file_db, years) {
  vars <- c(
    "year", "perwt", "met2013", "sex", "age", "race", "hispan", "educd",
    "degfield", "occ2010", "wkswork2", "uhrswork", "incwage"
  )
  data <- acs::acs_db_read(file_db, years = years, vars = vars)
  data <- acs::acs_clean(data)
  left_join(data, acs::cw_occ, by = c("occ2010" = "occ_code"))
}

calc_stats <- function(data, by = NULL) {
  if (!is.null(by)) {
    by_ <- syms(by)
    data <- group_by(data, !!!by_)
  }
  probs <- seq(0.1, 0.9, 0.01)

  out <- data %>%
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
    scale_y_log10(breaks = seq(2e4, 5e5, 2e4), minor_breaks = NULL, labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)
data$incwage <- data$incwage * wage_fix

data2 <- filter(data, sex == "male", age %in% 25:35, incwage > 7500)

plot_metros <- function(data, metros) {
  all <- calc_stats(data) %>%
    mutate(met2013 = "all")

  calc_stats(data, by = "met2013") %>%
    filter(grepl(!!metros, met2013)) %>%
    bind_rows(all) %>%
    mutate(met2013 = substr(met2013, 1, 15)) %>%
    mutate(met2013 = reorder(met2013, desc(q))) %>%
    plot_latest(color = "met2013")
}

plot_metros(data = data2, metros = "seattle|harrisburg|las vegas")

plot_occ_cats <- function(data, occ_cats) {
  all <- calc_stats(data) %>%
    mutate(occ_cat_name = "all")

  calc_stats(data, by = "occ_cat_name") %>%
    filter(grepl(!!occ_cats, occ_cat_name)) %>%
    bind_rows(all) %>%
    mutate(occ_cat_name = substr(occ_cat_name, 1, 15)) %>%
    mutate(occ_cat_name = reorder(occ_cat_name, desc(q))) %>%
    plot_latest(color = "occ_cat_name")
}

plot_occ_cats(data = data2, occ_cats = "computer|business")

plot_occs <- function(data, occs) {
  all <- calc_stats(data) %>%
    mutate(occ_name = "all")

  calc_stats(data, by = "occ_name") %>%
    filter(grepl(!!occs, occ_name)) %>%
    bind_rows(all) %>%
    mutate(occ_name = substr(occ_name, 1, 15)) %>%
    mutate(occ_name = reorder(occ_name, desc(q))) %>%
    plot_latest(color = "occ_name")
}

plot_occs(data = data2, occs = "software|computer prog|computer sci|database")

plot_degrees <- function(data, degrees) {
  all <- calc_stats(data) %>%
    mutate(degfield = "all")

  calc_stats(data, by = "degfield") %>%
    filter(grepl(!!degrees, degfield)) %>%
    bind_rows(all) %>%
    mutate(degfield = substr(degfield, 1, 15)) %>%
    mutate(degfield = reorder(degfield, desc(q))) %>%
    plot_latest(color = "degfield")
}

plot_degrees(data = data2, degrees = "computer|math|business")

by <- "met2013"

calc_stats(data2, by = by) %>%
  filter(p >= 0.4 & p <= 0.6) %>%
  group_by(!!sym(by)) %>%
  summarise(n = mean(n), pop = mean(pop), q = round(mean(q), -3)) %>%
  mutate(!!sym(by) := substr(!!sym(by), 1, 20)) %>%
  mutate(percent = round(pop / sum(pop) * 100, 1)) %>%
  arrange(desc(q)) %>%
  mutate(perc_cume = cumsum(percent))
