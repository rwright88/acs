#' Calculate distributions
#'
#' Calculate distribution of by2 within by1 groups and the distriubution of by2
#' within the overall population. Also calculate mean or median earnings by
#' group.
#'
#' @param data ACS IPUMS data.
#' @param by1 Character vector of groups.
#' @param by2 Character vector of groups.
#' @param type Type of earnings statistic by group (mean or median).
#' @return Data frame.
calc_dists <- function(data, by1, by2, type = c("mean", "median")) {
  by1s <- syms(by1)
  by2s <- syms(by2)
  type <- match.arg(type)
  n_years <- length(unique(data[["year"]]))

  dists_tot <- data %>%
    group_by(!!!by2s) %>%
    summarise(pop = sum(perwt) / n_years) %>%
    ungroup() %>%
    mutate(perc_all = pop / sum(pop) * 100) %>%
    select(-pop)

  if (type == "mean") {
    fun <- weighted.mean
  } else if (type == "median") {
    fun <- function(x, w) {
      Hmisc::wtd.quantile(x, w, probs = 0.5)
    }
  }

  dists <- data %>%
    group_by(!!!by1s, !!!by2s) %>%
    summarise(
      n = n(),
      pop = sum(perwt) / n_years,
      earn_stat = round(fun(incearn, perwt), -3)
    ) %>%
    group_by(!!!by1s) %>%
    mutate(percent = pop / sum(pop) * 100) %>%
    ungroup() %>%
    mutate(earn_stat = if_else(n < 100, NA_real_, earn_stat)) %>%
    left_join(dists_tot, by = by2) %>%
    arrange(!!!by1s, desc(pop))

  dists
}

#' Calculate earnings quantiles
#'
#' @param data ACS IPUMS data.
#' @param by Character vector of groups.
#' @return Data frame.
calc_q_earn <- function(data, by, probs) {
  bys <- syms(by)
  n_years <- length(unique(data[["year"]]))
  probsn <- str_c("p_", round(probs * 100))

  pop <- data %>%
    group_by(!!!bys) %>%
    summarise(
      n = n(),
      pop = sum(perwt) / n_years
    ) %>%
    ungroup() %>%
    mutate(perc = round(pop / sum(pop) * 100, 1))

  quantiles <- data %>%
    group_by(!!!bys) %>%
    summarise(
      p = list(probsn),
      q = list(round(Hmisc::wtd.quantile(incearn, perwt, probs = probs), -3))
    ) %>%
    unnest() %>%
    ungroup() %>%
    spread(p, q)

  quantiles <- pop %>%
    left_join(quantiles, by = by)

  for (x in probsn) {
    quantiles[[x]] <- if_else(quantiles[["n"]] < 100, NA_real_, quantiles[[x]])
  }

  quantiles
}
