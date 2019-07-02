# trends

library(acs)
library(dplyr)
library(ggplot2)
library(readr)
library(rwmisc)

years <- c(1990, 2000, 2010:2017)

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
    data <- filter(data, age %in% 25:55)
    # data$age <- round((data$age + 0.1) / 10) * 10
    data <- rec_occup(data)
    # data$coder <- (data$occ2010 %in% c(1000, 1010, 1020, 1060))

    by1 <- c("year", "sex", "occ_cat_name")
    by2 <- c("year", "sex")

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
  mutate_at(data, c("occ_cat_name", "occ_name"), tolower)
}

plot_trend <- function(data, y, color, facet = NULL) {
  y_ <- sym(y)
  color_ <- sym(color)

  out <- data %>%
    ggplot(aes(year, !!y_, color = !!color_)) +
    # geom_point(size = 2) +
    geom_line(size = 1) +
    scale_x_continuous(minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()

  if (!is.null(facet)) {
    out <- out + facet_wrap(facet)
  }

  out
}

# run ---------------------------------------------------------------------

res <- calc_by_year(years = years)

plot_custom <- function(res) {
  res2 <- res %>%
    mutate(occ_cat_name = substr(occ_cat_name, 1, 20)) %>%
    mutate(occ_cat_name = reorder(occ_cat_name, desc(percent)))

  res3 <- res2 %>%
    filter(year %% 10 == 0 | year == max(year))

  res2 %>%
    plot_trend(y = "percent", color = "sex", facet = "occ_cat_name") +
    geom_point(data = res3, mapping = aes(year, percent, color = sex), size = 2) +
    ggrepel::geom_text_repel(
      data = res3,
      mapping = aes(label = format(round(percent, 1), nsmall = 1)),
      size = 3,
      nudge_y = 1,
      direction = "y"
    ) +
    scale_y_continuous(breaks = seq(0, 100, 5), minor_breaks = NULL) +
    coord_cartesian(ylim = c(0, 15))
}

plot_custom(res)

ggsave("~/trends.png", dpi = 300, width = 10, height = 8)
