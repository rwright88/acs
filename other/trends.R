# trends

library(acs)
library(dplyr)
library(ggplot2)
library(rwmisc)
library(vroom)

years <- c(1990, 2000, 2010:2017)
wage_fix <- 1.05
file_pcepi <- "other/pcepi.csv"

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
    data <- rec_occ(data)

    by1 <- c("year", "sex", "occ_cat_name")
    by2 <- c("year", "sex")

    out <- group_by(data, !!!syms(by1))
    out <- summarise(out, n = n(), pop = sum(perwt))
    out <- group_by(out, !!!syms(by2))
    out <- mutate(out, percent = pop / sum(pop) * 100)
    out <- ungroup(out)

    wage <- data[which(data$incwage > 0), ]
    wage <- group_by(wage, !!!syms(by1))
    wage <- summarise(wage, wage_p50 = rwmisc::wtd_quantile(incwage, perwt, probs = 0.5))
    wage <- ungroup(wage)

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

inflate <- function(x, year, file_pcepi) {
  pcepi <- suppressMessages(vroom(file_pcepi))
  pcepi_val <- pcepi$pcepi[match(year, pcepi$year)]
  pcepi_cur <- max(pcepi_val)
  x * pcepi_cur / pcepi_val
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
    out <- out + facet_wrap(facet, ncol = 6)
  }

  out
}

# run ---------------------------------------------------------------------

res <- calc_by_year(years = years)
res$wage_p50 <- res$wage_p50 * wage_fix
res$wage_p50 <- inflate(res$wage_p50, res$year, file_pcepi)

rwmisc::summary2(res)

res %>%
  filter(year %in% c(1990, 2000, 2010, 2017)) %>%
  mutate(occ_cat_name = substr(occ_cat_name, 1, 20)) %>%
  mutate(occ_cat_name = reorder(occ_cat_name, desc(percent))) %>%
  plot_trend(y = "percent", color = "sex", facet = "occ_cat_name") +
  ggrepel::geom_text_repel(aes(label = round(percent, 1)), size = 3) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 100, 5), minor_breaks = NULL)
  # ggrepel::geom_text_repel(aes(label = round(wage_p50 / 1e3)), size = 3) +
  # scale_y_log10(breaks = seq(2e4, 2e5, 2e4), minor_breaks = NULL) +
  # coord_cartesian(ylim = c(1e4, 1e5))
