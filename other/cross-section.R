# cross section

library(acs)
library(dplyr)
library(ggplot2)
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
    scale_y_log10(breaks = seq(2e4, 2e5, 2e4), minor_breaks = NULL, labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)

data2 <- filter(data, sex == "male", age %in% 25:35, incwage > 7500)

all <- data2 %>%
  calc_stats() %>%
  mutate(met2013 = "all")

data2 %>%
  calc_stats(by = "met2013") %>%
  filter(grepl("new york|harrisburg|las vegas", met2013)) %>%
  bind_rows(all) %>%
  mutate(met2013 = substr(met2013, 1, 15)) %>%
  mutate(met2013 = reorder(met2013, desc(q))) %>%
  plot_latest(color = "met2013") +
  geom_hline(aes(yintercept = 60000), linetype = "dashed") +
  geom_hline(aes(yintercept = 60000 * 1.3), linetype = "dashed")
