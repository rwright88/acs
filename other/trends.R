# trends

library(tidyverse)
library(acs)

years <- 2010:2017

# funs --------------------------------------------------------------------

calc_by_year <- function(years) {
  if (!all(years %in% c(1980, 1990, 2000, 2010:2017))) {
    stop("years is out of range.")
  }

  out <- lapply(years, function(year) {
    file_db <- "~/data/acs/acsdb"
    vars <- c(
      "year", "perwt", "sex", "age", "race", "hispan", "educd", "degfield",
      "occ2010", "incearn"
    )

    data <- acs_db_read(file_db = file_db, years = year, vars = vars)
    data <- acs_clean(data)
    data <- filter(data, age %in% 30:49)

    out <- data %>%
      group_by(year, sex, educd, degfield) %>%
      summarise(n = n(), pop = sum(perwt), earn_mean = weighted.mean(incearn, perwt)) %>%
      group_by(year, sex) %>%
      mutate(d = pop / sum(pop)) %>%
      ungroup()

    out[, c("year", "sex", "educd", "degfield", "n", "pop", "d", "earn_mean")]
  })

  out <- bind_rows(out)
  out
}

rec_occupation <- function(data, file_occ = "data-raw/cw-occupation.csv") {
  cw_occ <- read_csv(file_occ, col_types = "icc")

  data <- data %>%
    left_join(cw_occ, by = c("occ2010" = "occ_code")) %>%
    mutate_at(c("occ_cat_name", "occ_name"), str_to_lower)

  data
}

plot_trend <- function(data, y, color, facet = NULL, n_col = NULL) {
  y <- sym(y)
  color <- sym(color)

  p <- data %>%
    ggplot(aes(year, !!y, color = !!color)) +
    geom_point(size = 1.5) +
    geom_line(size = 1) +
    ggrepel::geom_text_repel(
      mapping = aes(label = format(round(!!y * 100, 1), nsmall = 1)),
      data = filter(data, year %in% c(min(year), max(year))),
      size = 3.5,
      box.padding = 0.5,
      direction = "y",
      show.legend = FALSE
    ) +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_continuous(limits = c(0, NA), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    rwmisc::theme_rw()

  if (!is.null(facet)) {
    p <- p + facet_wrap(facet, ncol = n_col)
  }

  p
}

# run ---------------------------------------------------------------------

system.time({
  res <- calc_by_year(years = years)
})

rwmisc::summary2(res)

# res %>%
#   filter(year == max(year), sex == "male") %>%
#   mutate(percent = round(d * 100, 2)) %>%
#   arrange(desc(pop))
#
# res %>%
#   filter(year %in% c(2010, 2017), sex == "male") %>%
#   mutate(percent = round(d * 100, 2)) %>%
#   select(year, sex, educd, degfield, percent) %>%
#   spread(year, percent) %>%
#   mutate(diff = round(`2017` - `2010`, 2)) %>%
#   arrange(desc(`2017`))
