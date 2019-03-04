# trends

library(tidyverse)
library(acs)

# funs --------------------------------------------------------------------

calc_byyear <- function(years) {
  if (!all(years %in% c(1980, 1990, 2000, 2010:2017))) {
    stop("years is out of range.")
  }

  out <- lapply(years, function(.x) {
    file_db <- "~/data/acs/acsdb"

    vars <- c(
      "year", "perwt", "sex", "age", "race", "hispan", "educd", "degfield",
      "occ2010", "wkswork2", "uhrswork", "incearn"
    )

    data <- acs_db_read(file_db = file_db, years = .x, vars = vars)
    data <- acs_clean(data)
    data <- filter(data, age %in% 25:54, uhrswork >= 20, str_detect(wkswork2, "27|40|48|50"))

    out <- calc_earnq(data, by = c("year", "sex", "age", "race"))
    out
  })

  out <- bind_rows(out)
  out
}

calc_earnq <- function(data, by) {
  by <- syms(by)
  probs <- seq(0.1, 0.9, by = 0.1)

  out <- data %>%
    group_by(!!!by) %>%
    summarise(
      n = n(),
      pop = sum(perwt),
      p = list(probs),
      q = list(Hmisc::wtd.quantile(incearn, perwt, probs = probs, na.rm = TRUE))
    ) %>%
    unnest() %>%
    ungroup()

  out
}

rec_age_group <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x %in% 25:34] <- "25-34"
  out[x %in% 35:44] <- "35-44"
  out[x %in% 45:54] <- "45-54"
  out
}

rec_coder <- function(x) {
  coder <- str_c("computer sci|computer prog|software|database")
  out <- rep(NA_character_, length(x))
  out[str_detect(x, coder)] <- "coder"
  out[!str_detect(x, str_c(coder, "|unemp"))] <- "other"
  out[str_detect(x, "unemp")] <- "unemployed"
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
    geom_line(size = 1) +
    ggrepel::geom_text_repel(
      mapping = aes(label = format(round(!!y, 1), nsmall = 1)),
      data = filter(data, year %in% c(min(year), max(year))),
      size = 3,
      box.padding = 0.5,
      direction = "y",
      show.legend = FALSE
    ) +
    scale_x_continuous(minor_breaks = NULL) +
    scale_y_continuous(limits = c(0, NA), minor_breaks = NULL) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()

  if (!is.null(facet)) {
    p <- p + facet_wrap(facet, ncol = n_col)
  }

  p
}

plot_age <- function(data, y, color, facet) {
  y <- sym(y)
  color <- sym(color)

  p <- data %>%
    ggplot(aes(age, !!y, color = !!color)) +
    geom_point() +
    geom_smooth(method = "loess", span = 1, se = FALSE, size = 0.5) +
    scale_y_continuous(breaks = seq(0, 3e5, 2e4), limits = c(0, NA), labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    facet_wrap(facet) +
    theme_bw()

  p
}

# run ---------------------------------------------------------------------

system.time({
  res <- calc_byyear(years = 2017)
})

res

res %>%
  filter(
    year == max(year),
    race == "white",
    round(p * 10) %in% c(1, 3, 5, 7, 9)
  ) %>%
  mutate(p = reorder(p, desc(q))) %>%
  plot_age(y = "q", color = "p", facet = "sex")
