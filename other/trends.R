# trends

library(acs)
library(dplyr)
library(ggplot2)
library(rwmisc)
library(vroom)

years <- c(2010:2017)
wage_fix <- 1.05
file_pcepi <- "other/pcepi.csv"

# funs --------------------------------------------------------------------

calc_by_year <- function(years) {
  stopifnot(all(years %in% c(1980, 1990, 2000, 2010:2017)))

  out <- lapply(years, function(year) {
    file_db <- "~/data/acs/acsdb"
    vars <- c(
      "year", "perwt", "met2013", "sex", "age", "race", "hispan", "educd",
      "degfield", "occ2010", "incwage"
    )
    data <- acs::acs_db_read(file_db, years = year, vars = vars)
    data <- acs::acs_clean(data)
    data <- data[which(data$age %in% 25:54), ]
    data$age <- round((data$age + 0.1) / 10) * 10

    by1 <- c("year", "sex", "age", "race")
    by2 <- c("year", "sex", "age")

    out <- group_by(data, !!!syms(by1))
    out <- summarise(out, n = n(), pop = sum(perwt))
    out <- group_by(out, !!!syms(by2))
    out <- mutate(out, percent = pop / sum(pop) * 100)
    out <- ungroup(out)

    p <- seq(0.7, 0.8, 0.01)
    wage <- data[which(data$incwage > 0), ]
    wage <- group_by(wage, !!!syms(by1))
    wage <- summarise(wage,
      wage = mean(rwmisc::wtd_quantile(incwage, perwt, probs = !!p))
    )
    wage <- ungroup(wage)

    out <- left_join(out, wage, by = by1)
    out$wage[out$n < 100] <- NA
    out
  })

  bind_rows(out)
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

# run ---------------------------------------------------------------------

res <- calc_by_year(years = years)
res$wage <- res$wage * wage_fix
res$wage <- inflate(res$wage, res$year, file_pcepi)

rwmisc::summary2(res)

res %>%
  filter(sex == "male", race != "other") %>%
  mutate(race = reorder(race, desc(wage))) %>%
  ggplot(aes(year, wage, color = race)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  facet_wrap("age", nrow = 1) +
  scale_x_continuous(minor_breaks = NULL) +
  scale_y_log10(breaks = seq(1e4, 5e5, 1e4), minor_breaks = NULL) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_bw()
