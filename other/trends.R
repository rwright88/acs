# trends

library(tidyverse)
library(acs)
library(rwmisc)

years <- c(1990, 2000, 2010, 2017)

# funs --------------------------------------------------------------------

calc_by_year <- function(years) {
  stopifnot(all(years %in% c(1980, 1990, 2000, 2010:2017)))

  out <- lapply(years, function(.x) {
    file_db <- "~/data/acs/acsdb"
    vars <- c(
      "year", "perwt", "met2013", "sex", "age", "race", "hispan",
      "educd", "degfield", "occ2010", "uhrswork", "incwage"
    )
    data <- acs::acs_db_read(file_db, years = .x, vars = vars)
    data <- acs::acs_clean(data)
    data <- filter(data, age %in% 25:54)
    data$age <- round((data$age + 0.1) / 10) * 10
    data <- rec_occup(data)
    # data$coder <- (data$occ2010 %in% c(1000, 1010, 1020, 1060))

    by1 <- c("year", "sex", "age", "occ_cat_name")
    by2 <- c("year", "sex", "age")

    out <- data %>%
      group_by(!!!syms(by1)) %>%
      summarise(n = n(), pop = sum(perwt)) %>%
      group_by(!!!syms(by2)) %>%
      mutate(percent = pop / sum(pop) * 100) %>%
      ungroup()

    wage <- data %>%
      filter(incwage > 0) %>%
      group_by(!!!syms(by1)) %>%
      summarise(wage_p50 = rwmisc::wtd_quantile(incwage, perwt, probs = 0.5)) %>%
      ungroup()

    out <- left_join(out, wage, by = by1)
    out$wage_p50[out$n < 100] <- NA
    out
  })

  bind_rows(out)
}

rec_occup <- function(data) {
  cw_occ <- read_csv("data-raw/cw-occupation.csv", col_types = "icc")
  data <- left_join(data, cw_occ, by = c("occ2010" = "occ_code"))
  mutate_at(data, c("occ_cat_name", "occ_name"), str_to_lower)
}

plot_trend <- function(data, y, color, facet = NULL) {
  y_ <- sym(y)
  color_ <- sym(color)

  out <- data %>%
    ggplot(aes(year, !!y_, color = !!color_)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()

  if (!is.null(facet)) {
    out <- out + facet_wrap(facet)
  }

  out
}

# run ---------------------------------------------------------------------

res <- calc_by_year(years = years)

res %>%
  filter(age == 30) %>%
  mutate(occ_cat_name = str_sub(occ_cat_name, 1, 20)) %>%
  mutate(occ_cat_name = reorder(occ_cat_name, desc(percent))) %>%
  plot_trend(y = "percent", color = "sex", facet = "occ_cat_name") +
  ggrepel::geom_text_repel(aes(label = format(round(percent, 1), nsmall = 1)), size = 3, nudge_y = 1, direction = "y") +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(breaks = seq(0, 100, 5), minor_breaks = NULL) +
  coord_cartesian(ylim = c(0, 15))

ggsave("~/trends.png", dpi = 300, width = 10, height = 8)
