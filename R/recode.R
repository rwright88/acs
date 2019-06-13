# recoding variables notes:
# age:      ages 90 and above vary by year of data, assume NA
# educd:    ???
# uhrswork: 99 is a top code

# age ipums variable
rec_age <- function(x) {
  val <- (x %in% 0:89)
  out <- rep(NA_integer_, length(x))
  out[val] <- x[val]
  out
}

# degfield ipums variable
rec_degree <- function(x) {
  code <- cw_degree[["degree_code"]]
  name <- cw_degree[["degree_name"]]
  out <- rep(NA_character_, length(x))
  out <- name[match(x, code)]
  out
}

# educd ipums variable
rec_education <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x %in% 0:59]    <- "less-hs"
  out[x %in% 60:64]   <- "hs-ged"
  out[x %in% 65:99]   <- "assoc-some"
  out[x %in% 100:109] <- "bachelor"
  out[x %in% 110:119] <- "advanced"
  out
}

# marst ipums variable
rec_married <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x %in% 1:2] <- TRUE
  out[x %in% 3:6] <- FALSE
  out
}

# met2013 ipums variable
rec_metro <- function(x) {
  code <- cw_metro[["msa_code"]]
  name <- cw_metro[["msa_name"]]
  out <- rep(NA_character_, length(x))
  out <- name[match(x, code)]
  out
}

# race and hispan ipums variables
rec_race <- function(race, hisp) {
  oth <- c(3, 7:9)
  out <- rep(NA_character_, length(race))
  out[race == 1     & hisp == 0]     <- "white"
  out[race == 2     & hisp == 0]     <- "black"
  out[race %in% 4:6 & hisp == 0]     <- "asian-pi"
  out[race %in% oth & hisp == 0]     <- "other"
  out[                hisp %in% 1:4] <- "hispanic"
  out
}

# sex ipums variable
rec_sex <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x == 1] <- "male"
  out[x == 2] <- "female"
  out
}

# trantime ipums variable
rec_travel <- function(x) {
  val <- (x %in% 1:999)
  out <- rep(NA_integer_, length(x))
  out[val] <- x[val]
  out
}

# incwage ipums variable
rec_wage <- function(x) {
  x[x == 999998] <- NA
  x[x == 999999] <- 0
  x
}

# classwkr ipums variable
rec_work_class <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x == 1] <- "self employed"
  out[x == 2] <- "wage salary"
  out
}

# empstat ipums variable
rec_work_employ <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x == 1]         <- "employed"
  out[x == 2]         <- "unemployed"
  out[x %in% c(0, 3)] <- "not in lf"
  out
}

# uhrswork ipums variable
rec_work_hours <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[x %in% 0:98] <- x[x %in% 0:98]
  out
}

# wkswork2 ipums variable
rec_work_weeks <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x == 0] <- "0 weeks"
  out[x == 1] <- "1-13 weeks"
  out[x == 2] <- "14-26 weeks"
  out[x == 3] <- "27-39 weeks"
  out[x == 4] <- "40-47 weeks"
  out[x == 5] <- "48-49 weeks"
  out[x == 6] <- "50-52 weeks"
  out
}
