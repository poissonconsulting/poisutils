#' Standardise Datetime
#'
#' Corrects for any time zone and day light savings difference in a date-time object
#' and returns as tz = 'UTC' but shifted by the standardised_offset. Depending
#' on the logging device recorded date times are often in different time zones,
#' i.e., Vemco acoustic receiver detections (UTC), Onset
#' temperature logger readings (PST or PDT depending on when deployed)
#' versus watch (PST or PDT depending on when recorded). To allow analyses to
#' be performed on the year, month, day, hour etc elements without any errors
#' it is important to standardise the date time objects so they have the
#' same time zone. The current function achieves this by forcing all the
#' date-times to tz UTC and then adjusting for the difference in hours between
#' the original \code{utc_offset} and the standardised offset.
#'
#' @param x a datetime object
#' @param standardised_offset an integer scalar indicating the time difference
#' in hours between UTC and the desired time zone.
#' @return A standardised date-time object.
#' @seealso \code{\link{utc_offset}} \code{\link{timezones}}
#' @importFrom lubridate force_tz new_period
#' @export
#' @examples
#'
#' vemco <- as.POSIXct(c("2000-04-01 17:00:00"), tz = "UTC")
#' watch <- as.POSIXct(c("2000-04-01 9:00:00", "2000-04-02 10:00:00"), tz = "PST8PDT")
#' logger <- as.POSIXct(c("2000-04-01 9:00:00", "2000-04-02 9:00:00"), tz = "Etc/GMT+8")
#'
#' standardise_datetime(vemco)
#' standardise_datetime(watch)
#' standardise_datetime(logger)
standardise_datetime <- function(x, standardised_offset = -8) {
  if (!is_integer_scalar(standardised_offset) || !is_bounded(standardised_offset,
                                                             -12, 12))
    stop("standardised_offset must be an integer scalar between -12 and +12")

  offset <- utc_offset(x)
  x <- force_tz(x, tzone = "UTC")
  x <- x - new_period(hour = offset - standardised_offset)
  return(x)
}
