#' Extracts datetime from data frame
#'
#' Extracts datetime from data frame
#'
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Year, Month, Day, Hour, Minute and/or Second
#' @param tz string of time zone. Typical values are
#' 'PST8PDT' if Pacific time i.e., includes daylight savings,
#' 'PST' if Pacific standard time i.e. no daylight savings
#' 'PDT' if Pacific dayight time i.e. all daylight savings
#' and 'UTC' all in  Universal Coordinated Time (GMT).
#' 'PST8PDT' occurs when times are read from a watch,
#' 'PST' occurs when loggers are deployed in pacific standard time (recommended)
#' 'PDT' occurs when loggers are deployed in pacific daylight time without
#' adjusting and 'UTC' occurs with non-adjusted Vemco receivers.
#' @param standardised_offset an integer scalar indicating the time difference
#' in hours between UTC and the desired time zone.
#' @return A date time object
#' @seealso \code{\link{ISOdatetime}}
#' and \code{\link{extract_date}}
#' @examples
#' data <- data.frame(Year = 2000, Month = 1:12, Day = 2,
#'                    Hour = 12, Minute = 30, Second = 1)
#' extract_datetime(data)
#' extract_date(data)
#' extract_time(data)
#'
#' extract_datetime(data)
#' extract_datetime(data, tz = 'PDT')
#' extract_datetime(data, tz = 'PST8PDT')
#' extract_datetime(data, tz = 'UTC')
#' extract_datetime(data, standardised_offset = -7)
#'
#' data <- data.frame(ReleaseYear = 2002, ReleasedMonth = 2, DayRel = 2,
#'                    RelHourRel = 2, Minute = 2, Second = 2, DupSecond = 3)
#'
#' extract_datetime(data, '')
#' extract_datetime(data, '', '')
#'
#' @importFrom lubridate hours
#' @export
extract_datetime <- function(data, prefix = "^", suffix = "$", expand = c("Year",
                                                                          "Month", "Day", "Hour", "Minute", "Second"), tz = "PST", standardised_offset = -8) {

  assert_that(is.string(prefix) && noNA(prefix))
  assert_that(is.string(suffix) && noNA(suffix))
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Year", "Month", "Day", "Hour", "Minute", "Second")))
  assert_that(!any(duplicated(expand)))
  assert_that(is.string(tz) && noNA(tz))

  values <- list()

  values[["Year"]] <- 2000
  values[["Month"]] <- 1
  values[["Day"]] <- 1
  values[["Hour"]] <- 0
  values[["Minute"]] <- 0
  values[["Second"]] <- 0

  colnames <- colnames(data)
  for (x in expand) {
    regexp <- paste0(prefix, x, suffix)
    index <- grep(regexp, colnames)
    if (length(index) == 0) {
      warning("Regular expression ", regexp, " not in colnames")
    } else if (length(index) == 1) {
      values[[x]] <- data[[colnames[index]]]
    } else warning("Regular expression ", regexp, " matches columns ", paste(colnames[index],
                                                                             collapse = " "))
  }

  adjust <- switch(tz, PDT = -7, PST = -8, 0)

  if (adjust != 0)
    tz <- "UTC"

  dt <- ISOdatetime(year = values[["Year"]], month = values[["Month"]], day = values[["Day"]],
                    hour = values[["Hour"]], min = values[["Minute"]], sec = values[["Second"]],
                    tz = tz)

  if (adjust != 0) {
    dt <- dt - hours(adjust)
  }

  standardise_datetime(dt, standardised_offset = standardised_offset)
}

#' Extracts Date
#'
#' Extracts Date
#'
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Year, Month, Day
#' @return A Date object
#' @seealso \link{extract_datetime}
#' @export
extract_date <- function(data, prefix = "", suffix = "", expand = c("Year", "Month",
                                                                    "Day")) {

  assert_that(is.string(prefix) && noNA(prefix))
  assert_that(is.string(suffix) && noNA(suffix))
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Year", "Month", "Day")))
  assert_that(!any(duplicated(expand)))

  values <- list()

  values[["Year"]] <- 2000
  values[["Month"]] <- 1
  values[["Day"]] <- 1

  colnames <- colnames(data)
  for (x in expand) {
    regexp <- paste0(prefix, x, suffix)
    index <- grep(regexp, colnames)
    if (length(index) == 0) {
      warning("Regular expression", regexp, "not in colnames")
    } else if (length(index) == 1) {
      values[[x]] <- data[[colnames[index]]]
    } else warning("Regular expression", regexp, "matches columns", colnames[index])
  }
  char <- paste(values[["Year"]], values[["Month"]], values[["Day"]], sep = "-")
  is.na(char[grep("NA", char)]) <- TRUE
  as.Date(char)
}

#' Extracts time from data frame
#'
#' Extracts time from data frame
#'
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Hour, Minute and/or Second
#' @param tz string of time zone
#' @param standardised_offset an integer scalar indicating the time difference
#' in hours between UTC and the desired time zone.
#' @return A datetime object
#' @export
extract_time <- function(data, prefix = "", suffix = "", expand = c("Hour", "Minute",
                                                                    "Second"), tz = "PST", standardised_offset = -8) {

  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Hour", "Minute", "Second")))
  assert_that(!any(duplicated(expand)))

  extract_datetime(data, prefix, suffix, expand, tz, standardised_offset)
}
