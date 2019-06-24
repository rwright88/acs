# trends

library(tidyverse)
library(acs)

years <- 2010:2017

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
    data <- filter(data, age %in% 25:35)
    # data <- rec_occup(data)
    data$coder <- (data$occ2010 %in% c(1000, 1010, 1020, 1060))

    by1 <- c("year", "sex", "race", "coder")
    by2 <- c("year", "sex", "race")

    out <- data %>%
      group_by(!!!syms(by1)) %>%
      summarise(n = n(), pop = sum(perwt)) %>%
      group_by(!!!syms(by2)) %>%
      mutate(percent = pop / sum(pop) * 100) %>%
      ungroup()

    wage <- data %>%
      filter(incwage > 0) %>%
      group_by(!!!syms(by1)) %>%
      summarise(wage_p50 = Hmisc::wtd.quantile(incwage, perwt, probs = 0.5)) %>%
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
  data <- mutate_at(data, c("occ_cat_name", "occ_name"), str_to_lower)
  data
}

plot_trend <- function(data, y, color, facet = NULL) {
  y_ <- sym(y)
  color_ <- sym(color)

  p <- data %>%
    ggplot(aes(year, !!y_, color = !!color_)) +
    geom_point(size = 2) +
    geom_line(size = 1) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()

  if (!is.null(facet)) {
    p <- p + facet_wrap(facet)
  }

  p
}

# run ---------------------------------------------------------------------

res <- calc_by_year(years = years)

res %>%
  filter(race != "other", coder == TRUE) %>%
  plot_trend(y = "percent", color = "race", facet = "sex")
