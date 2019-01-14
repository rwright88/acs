#' Clean IPUMS ACS data
#'
#' @param data Data frame of IPUMS ACS data.
#' @return Data frame.
#' @export
clean_data <- function(data) {
  if (!is.data.frame(data)) {
    stop("data must be a data frame.", call. = FALSE)
  }

  params <- dplyr::tribble(
    ~var,       ~fun,
    "age",      rec_age,
    "educd",    rec_education,
    "sex",      rec_sex,
    "classwkr", rec_work_class,
    "empstat",  rec_work_employ,
    "uhrswork", rec_work_hours,
    "wkswork2", rec_work_weeks
  )

  data   <- setNames(data, tolower(names(data)))
  vars   <- names(data)
  params <- dplyr::filter(params, var %in% vars)

  for (i in seq_along(params[["var"]])) {
    var <- params[["var"]][[i]]
    fun <- params[["fun"]][[i]]
    data[[var]] <- fun(data[[var]])
  }

  # for now, for functions with more than 1 arguement
  if ("race" %in% vars && "hispan" %in% vars) {
    data[["race"]] <- rec_race(data[["race"]], data[["hispan"]])
    data[["hispan"]] <- NULL
  }

  data
}
