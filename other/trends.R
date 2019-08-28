# trends

library(acs)
library(dplyr)
library(ggplot2)
library(rwmisc)

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
    data$educd <- rec_edu(data$educd)
    data <- rec_occ(data)

    by1 <- c("year", "sex", "educd")
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

rec_edu <- function(x) {
  x[x %in% c("hs-ged", "less-hs")] <- "hs-or-less"
  x[x %in% c("advanced", "bachelor")] <- "bach-plus"
  x
}

rec_occ <- function(data) {
  left_join(data, acs::cw_occ, by = c("occ2010" = "occ_code"))
}

plot_trend <- function(data, y, color, facet = NULL) {
  y_ <- sym(y)
  color_ <- sym(color)

  out <- data %>%
    ggplot(aes(year, !!y_, color = !!color_)) +
    geom_point(size = 2) +
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

res %>%
  mutate(educd = factor(educd, c("hs-or-less", "assoc-some", "bach-plus"))) %>%
  plot_trend(y = "percent", color = "sex", facet = "educd") +
  ggrepel::geom_text_repel(aes(label = round(percent, 1)), size = 3) +
  scale_y_continuous(limits = c(0, NA))
