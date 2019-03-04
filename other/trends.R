# trends

library(tidyverse)
library(acs)

years <- c(1990, 2000, 2010:2017)

# funs --------------------------------------------------------------------

calc_byyear <- function(years) {
  if (!all(years %in% c(1980, 1990, 2000, 2010:2017))) {
    stop("years is out of range.")
  }

  out <- lapply(years, function(.x) {
    file_db <- "~/data/acs/acsdb"
    vars <- c("year", "perwt", "sex", "age", "race", "hispan", "incearn")

    dat <- acs_db_read(file_db = file_db, years = .x, vars = vars)
    dat <- acs_clean(dat)

    dat <- dat %>%
      filter(age %in% 25:54)

    probs <- seq(0.1, 0.9, by = 0.1)

    pops <- dat %>%
      group_by(year, sex, age, race) %>%
      summarise(
        n = n(),
        pop = sum(perwt),
        p = list(probs),
        q = list(Hmisc::wtd.quantile(incearn, perwt, probs = probs, na.rm = TRUE))
      ) %>%
      unnest() %>%
      ungroup()

    ord <- c("year", "sex", "age", "race", "n", "pop", "p", "q")
    pops <- pops[ord]
    pops
  })

  out <- bind_rows(out)
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
    geom_line(size = 1.1) +
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
    p <- p + facet_wrap(vars(!!sym(facet)), ncol = n_col)
  }

  p
}

# run ---------------------------------------------------------------------

system.time({
  pops <- calc_byyear(years = 2017)
})

pops %>%
  filter(sex == "male", race != "other") %>%
  mutate(race = reorder(race, desc(earn_mean))) %>%
  ggplot(aes(age, earn_p50, color = race)) +
  geom_point(size = 1.5) +
  geom_smooth(span = 1, se = FALSE, size = 0.5) +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_bw()
