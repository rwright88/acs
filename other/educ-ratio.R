# education ratio

library(acs)
library(dplyr)
library(ggplot2)

years <- 2010:2017

# funs --------------------------------------------------------------------

calc_by_year <- function(years) {
  stopifnot(all(years %in% c(1980, 1990, 2000, 2010:2017)))

  out <- lapply(years, function(year) {
    file_db <- "~/data/acs/acsdb"
    vars <- c("year", "perwt", "sex", "age", "race", "hispan", "educd")
    data <- acs::acs_db_read(file_db, years = year, vars = vars)
    data <- acs::acs_clean(data)
    data <- data[which(data$age %in% 25:54), ]
    data$age <- round((data$age + 0.1) / 10) * 10
    data$educd <- rec_educ(data$educd)

    by1 <- c("year", "sex", "age", "race", "educd")
    by2 <- c("year", "sex", "age", "race")

    out <- group_by(data, !!!syms(by1))
    out <- summarise(out, n = n(), pop = sum(perwt))
    out <- group_by(out, !!!syms(by2))
    out <- mutate(out, percent = pop / sum(pop) * 100)
    out <- ungroup(out)
    out
  })

  bind_rows(out)
}

rec_educ <- function(x) {
  out <- rep("less-bs", length(x))
  out[x %in% c("bachelor", "advanced")] <- "bs-or-more"
  out
}

# run ---------------------------------------------------------------------

res <- calc_by_year(years = years)

res %>%
  select(-n, -pop) %>%
  filter(age == 40, race != "other") %>%
  tidyr::spread(sex, percent) %>%
  mutate(ratio = female / male) %>%
  ggplot(aes(year, ratio, color = educd)) +
  geom_point(size = 2) +
  geom_line(size = 1) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  facet_wrap("race", nrow = 1) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme_bw()
