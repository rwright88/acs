# TODO:
# Add more variables

#' Clean IPUMS ACS data
#'
#' Currently, this only cleans/recodes a selection of variables.
#'
#' @param data Data frame of IPUMS ACS data.
#' @return Data frame.
#' @export
acs_clean <- function(data) {
  stopifnot(is.data.frame(data))

  params <- dplyr::tribble(
    ~var,       ~fun,
    "age",      rec_age,
    "degfield", rec_degree,
    "educd",    rec_education,
    "marst",    rec_married,
    "met2013",  rec_metro,
    "sex",      rec_sex,
    "trantime", rec_travel,
    "incwage",  rec_wage,
    "classwkr", rec_work_class,
    "empstat",  rec_work_employ,
    "uhrswork", rec_work_hours,
    "wkswork2", rec_work_weeks
  )

  names(data) <- tolower(names(data))
  vars <- names(data)
  params <- params[params$var %in% vars, ]

  for (i in seq_along(params[["var"]])) {
    var <- params[["var"]][[i]]
    fun <- params[["fun"]][[i]]
    data[[var]] <- fun(data[[var]])
  }

  if ("race" %in% vars && "hispan" %in% vars) {
    data$race <- rec_race(data$race, data$hispan)
    data$hispan <- NULL
  }

  data
}
