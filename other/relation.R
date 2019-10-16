# Relation to head of household

library(acs)
library(dplyr)
library(ggplot2)

years <- c(1970, 1980, 1990, 2000, 2010, 2017)

# funs --------------------------------------------------------------------

calc_by_year <- function(years) {
  good <- c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2017)
  stopifnot(all(years %in% good))

  out <- lapply(years, function(year) {
    file_db <- "~/data/acs/acsdb2"
    vars <- c("year", "perwt", "sex", "age", "marst", "relate")
    data <- acs::acs_db_read(file_db, years = year, vars = vars)
    data <- acs::acs_clean(data)
    data <- data[which(data$age %in% 20:40), ]
    # data <- data[which(data$relate != "institutional inmates"), ]
    data$cond <- rec_cond(data$relate, data$marst)

    by1 <- c("year", "sex", "age", "cond")
    by2 <- c("year", "sex", "age")

    out <- group_by(data, !!!syms(by1))
    out <- summarise(out, n = n(), pop = sum(perwt))
    out <- group_by(out, !!!syms(by2))
    out <- mutate(out, percent = pop / sum(pop) * 100)
    ungroup(out)
  })

  bind_rows(out)
}

rec_cond <- function(relate, married) {
  rel <- c(
    "head",
    "spouse",
    "child",
    "child-in-law",
    "parent",
    "parent-in-law",
    "sibling",
    "sibling-in-law",
    "grandchild",
    "other relative",
    "partner, friend, visitor",
    "other non-relatives",
    "institutional inmates"
  )
  out <- rep("other", length(relate))
  out[relate %in% rel[1:2] & married == TRUE]          <- "married-head-spouse"
  out[relate %in% rel[c(1, 11:12)] & married == FALSE] <- "alone-or-non-rel"
  out[relate %in% rel[c(3:4, 9)]]                      <- "with-parents"
  out
}

# run ---------------------------------------------------------------------

res <- calc_by_year(years)

res %>%
  filter(cond == "married-head-spouse") %>%
  mutate(year = factor(year)) %>%
  ggplot(aes(age, percent, color = year)) +
  geom_point(size = 1.5) +
  geom_line(size = 1) +
  facet_wrap("sex") +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_continuous(limits = c(0, NA), breaks = seq(0, 100, 20)) +
  scale_color_viridis_d() +
  theme_bw()
