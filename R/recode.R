# recoding variables notes:
# age:      ages 90 and above vary by year of data, assume NA
# educd:    ???
# uhrswork: 99 is a top code

# age ipums variable
rec_age <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[x %in% 0:89] <- x[x %in% 0:89]
  out
}

# educd ipums variable
rec_education <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x %in% 10:59]   <- "less high"
  out[x %in% 60:64]   <- "high school"
  out[x %in% 65:99]   <- "some assoc"
  out[x %in% 100:109] <- "bachelor"
  out[x %in% 110:119] <- "advanced"
  out
}

# race and hispan ipums variables
rec_race <- function(race, hisp) {
  oth <- c(3, 7:9)
  out <- rep(NA_character_, length(race))
  out[race == 1     & hisp == 0]     <- "white"
  out[race == 2     & hisp == 0]     <- "black"
  out[race %in% 4:6 & hisp == 0]     <- "asian pi"
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
  out[x == 1] <- "employed"
  out[x == 2] <- "unemployed"
  out[x == 3] <- "not in lf"
  out
}

# uhrswork ipums variable
rec_work_hours <- function(x) {
  out <- rep(NA_integer_, length(x))
  out[x %in% 1:99] <- x[x %in% 1:99]
  out
}

# wkswork2 ipums variable
rec_work_weeks <- function(x) {
  out <- rep(NA_character_, length(x))
  out[x == 1] <- "1-13 weeks"
  out[x == 2] <- "14-26 weeks"
  out[x == 3] <- "27-39 weeks"
  out[x == 4] <- "40-47 weeks"
  out[x == 5] <- "48-49 weeks"
  out[x == 6] <- "50-52 weeks"
  out
}
