# distribution of hours and weeks worked

library(tidyverse)
library(acs)

# funs --------------------------------------------------------------------

calc_dist_hours <- function(data, groups) {
  groups <- syms(groups)

  dists <- data %>%
    mutate(uhrswork = ifelse(is.na(uhrswork), 0, uhrswork)) %>%
    mutate(uhrswork = round(uhrswork / 10) * 10) %>%
    group_by(!!!groups, uhrswork) %>%
    summarise(pop = sum(perwt)) %>%
    group_by(!!!groups) %>%
    mutate(p = pop / sum(pop)) %>%
    mutate(p_cume = cumsum(p)) %>%
    ungroup()

  dists
}

calc_dist_weeks <- function(data, groups) {
  groups <- syms(groups)

  dists <- data %>%
    mutate(wkswork2 = ifelse(is.na(wkswork2), 0, wkswork2)) %>%
    group_by(!!!groups, wkswork2) %>%
    summarise(pop = sum(perwt)) %>%
    group_by(!!!groups) %>%
    mutate(p = pop / sum(pop)) %>%
    mutate(p_cume = cumsum(p)) %>%
    ungroup()

  dists
}

plot_dist <- function(data, x, y, color, facet) {
  x <- sym(x)
  y <- sym(y)
  color <- sym(color)

  data %>%
    ggplot(aes(!!x, !!y, group = !!color, color = !!color)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    scale_y_continuous(limits = c(0, 1)) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    facet_wrap(facet) +
    theme_bw()
}

# run ---------------------------------------------------------------------

vars <- c("year", "perwt", "sex", "age", "race", "hispan", "wkswork2", "uhrswork", "incearn")

dat <- db_read_acs(file_db = "~/data/acs/acsdb", years = 2017, vars = vars)
dat <- clean_acs(dat)
dat <- filter(dat, age %in% 25:54)

dat %>%
  calc_dist_hours(groups = c("sex", "race")) %>%
  filter(race != "other", uhrswork <= 80) %>%
  plot_dist(x = "uhrswork", y = "p_cume", color = "race", facet = "sex")

dat %>%
  calc_dist_weeks(groups = c("sex", "race")) %>%
  filter(race != "other") %>%
  plot_dist(x = "wkswork2", y = "p_cume", color = "race", facet = "sex")
