# earnings percentiles

library(acs)
library(dplyr)
library(ggplot2)
library(rwmisc)
library(tidyr)

file_db <- "~/data/acs/acsdb"
years <- 2015:2017
wage_fix <- 1.1

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

  out <- summarise(out,
    n = n(),
    pop = sum(perwt),
    p = list(!!probs),
    q = list(rwmisc::wtd_quantile(incwage, perwt, probs = !!probs))
  )
  out <- unnest(out)
  out <- ungroup(out)
  out$q[out$n < 100] <- NA
  out
}

rank_it <- function(data, by, order_by, q_range) {
  out <- calc_stats(data, by = by)
  out <- out[which(out$p >= q_range[1] & out$p <= q_range[2]), ]
  out <- group_by(out, !!sym(by))
  out <- summarise(out, n = mean(n), pop = mean(pop), q = round(mean(q), -3))
  out <- ungroup(out)
  out[[by]] <- substr(out[[by]], 1, 30)
  out$percent <- out$pop / sum(out$pop) * 100
  out <- out[order(-out[[order_by]]), ]
  out$perc_cume <- cumsum(out$percent)
  out$percent <- round(out$percent, 1)
  out$perc_cume <- round(out$perc_cume, 1)
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
    geom_smooth(method = "loess", span = 0.5, se = FALSE, size = 1) +
    scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1), minor_breaks = NULL) +
    scale_y_log10(breaks = seq(2e4, 5e5, 2e4), minor_breaks = NULL, labels = scales::comma) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    theme_bw()
}

plot_it <- function(data, by, sub) {
  all <- calc_stats(data)
  all[[by]] <- "all"

  out <- calc_stats(data, by = by)
  out <- out[which(grepl(sub, out[[by]])), ]
  out <- dplyr::bind_rows(out, all)
  out[[by]] <- substr(out[[by]], 1, 15)
  out[[by]] <- reorder(out[[by]], desc(out$q))

  plot_latest(out, color = by)
}

# run ---------------------------------------------------------------------

data <- get_data(file_db, years)
data$incwage <- data$incwage * wage_fix

data2 <- filter(data, sex == "male", age %in% 25:55, incwage > 0)

rank_it(data2, by = "met2013", order_by = "q", q_range = c(0.4, 0.6))

plot_it(data2, by = "met2013",      sub = "seattle|harrisburg|las vegas")
plot_it(data2, by = "occ_cat_name", sub = "computer|construction|transport")
plot_it(data2, by = "occ_name",     sub = "software|computer prog|computer sci")
plot_it(data2, by = "degfield",     sub = "computer|business|math")
