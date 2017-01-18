#' Cleans up datetime from data frame
#'
#' @description
#' Cleans up datetime from data frame
#'
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string for matching regexp pattern start
#' @param suffix string for matching regexp pattern end
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Year, Month, Day, Hour, Minute and/or Second
#' \code{\link{grep}} function
#' @return Cleaned up data
#' @seealso \code{\link{extract_datetime}}
#' @examples
#' data <- data.frame(Year = 2000, Month = 1:4, Day = 2)
#' cleanup_datetime(data)
#' data <- data.frame(Year = 2000, Month = 1:4, Day = 2, Comments = 'No way')
#' cleanup_datetime(data)
#' data <- data.frame(YearRelease = 2000, MonthReleased = 1:4, DayDeceased = 2,
#'                    XDay = 3, TDayY = 4)
#' cleanup_datetime(data)
#' cleanup_datetime(data, '', '')
#' cleanup_datetime(data, '^X')
#' cleanup_datetime(data, '^T')
#' cleanup_datetime(data, '^T', '')
#' cleanup_datetime(data, '^T', 'Y$')
#' cleanup_datetime(data, ,'Release[d]*|Deceased$')
#'
#' @export
cleanup_datetime <- function(data, prefix = "^", suffix = "$", expand = c("Year",
                                                                          "Month", "Day", "Hour", "Minute", "Second")) {

  assert_that(is.string(prefix) && noNA(prefix))
  assert_that(is.string(suffix) && noNA(suffix))
  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Year", "Month", "Day", "Hour", "Minute", "Second")))
  assert_that(!any(duplicated(expand)))

  colnames <- colnames(data)
  indices <- integer(0)
  for (x in expand) {
    indices <- c(indices, grep(paste0(prefix, x, suffix), colnames))
  }
  if (length(indices))
    return(data[, -sort(unique(indices)), drop = FALSE])
  data
}

#' Cleans up date from data frame
#'
#' @description
#' Cleans up date from data frame
#'
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Year, Month, Day
#' @return Cleaned up data
#' @seealso \code{\link{cleanup_datetime}}
#' @examples
#' data <- data.frame(Year = 2000, Month = 1:4, Day = 2, Comments = 'No way')
#' cleanup_date(data)
#' @export
cleanup_date <- function(data, prefix = "", suffix = "", expand = c("Year", "Month",
                                                                    "Day")) {

  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Year", "Month", "Day")))
  assert_that(!any(duplicated(expand)))

  cleanup_datetime(data, prefix, suffix, expand)
}

#' Cleans up time from data frame
#'
#' @description
#' Cleans up time from data frame
#'
#' @param data data.frame with columns of [prefix]expand[suffix], [prefix]expand[suffix] values etc
#' @param prefix string
#' @param suffix string
#' @param expand character vector of column names where [prefix]expand[suffix]
#' and expand must only be Hour, Minute and/or Second
#' @return Cleaned up data
#' @seealso \code{\link{cleanup_datetime}}
#' @export
cleanup_time <- function(data, prefix = "", suffix = "", expand = c("Hour", "Minute",
                                                                    "Second")) {

  assert_that(is.character(expand) && noNA(expand))
  assert_that(all(expand %in% c("Hour", "Minute", "Second")))
  assert_that(!any(duplicated(expand)))

  cleanup_datetime(data, prefix, suffix, expand)
}
