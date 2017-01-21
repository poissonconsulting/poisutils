check_hobo_csv_data_colname <- function(colnames, pattern, which, file) {
  wch <- which(str_detect(colnames, pattern))
  if (!length(wch) || wch != which)
    error("column '", colnames[which], "' in file '", file, "', does match the regular expression '", pattern, "'")
}

check_hobo_csv_data_colnames <- function(data, file) {
  colnames <- colnames(data)
  check_hobo_csv_data_colname(colnames, "^#$", 1, file)
  check_hobo_csv_data_colname(colnames, "^Date Time, GMT", 2, file)
  check_hobo_csv_data_colname(colnames, "^Temp, ", 3, file)
  check_hobo_csv_data_colname(colnames, "^Coupler Detached [(]LGR S/N: ", 4, file)
  check_hobo_csv_data_colname(colnames, "^End Of File [(]LGR S/N: ", 5, file)
  data
}

check_hobo_csv_data <- function(data, file) {
  check_hobo_csv_data_colnames(data, file)
  data
}

extract_hobo_meta_data_temp_units <- function(colnames) {
  temp_units <- str_extract_all(colnames,  "(?<=Temp, )(.{1,2})(?= [(LGR])")[[1]]
  if (!all(temp_units == temp_units[1]))
    error("more than one unit in colnames in file '", file, "'")
  temp_units[1]
}

extract_hobo_meta_data_logger <- function(colnames) {
  logger <- str_extract_all(colnames,  "(?<=LGR S[/]N[:] )(\\d+)(?=,)")[[1]]
  if (!all(logger == logger[1]))
    error("more than one logger id in colnames in file '", file, "'")
  logger[1]
}

extract_hobo_meta_data_tz_offset <- function(colnames) {
  tz <- str_extract_all(colnames,  "(?<=Date Time, GMT)([^\n]{2,})(?=\n)")[[1]]
  if (!all(tz == tz[1]))
    error("more than one tz in colnames in file '", file, "'")
  tz[1]
}

extract_hobo_meta_data <- function(data) {
  colnames <- colnames(data) %>% str_c(collapse = "\n")

  data_frame(Logger = extract_hobo_meta_data_logger(colnames),
             TempUnits = extract_hobo_meta_data_temp_units(colnames),
             TimeZoneOffset = extract_hobo_meta_data_tz_offset(colnames))
}

filter_hobo_data <- function(data, file) {
  stopifnot(identical(colnames(data), c("FileRow", "DateTime", "Temperature", "CouplerDetached", "EndOfFile")))

  start <- which(data$CouplerDetached == "Logged") %>% max()
  end <- which(data$EndOfFile == "Logged")[1]

  if ((start + 2) >= end) {
    warning("no logged data in file '", file, "'")
    return(slice_(data, 0))
  }

  data %<>% slice_(~(start+1):(end-1))
  data %<>% filter_(~!is.na(Temperature))
  data
}

read_hobo_csv_file <- function(file, orders, temp_units, utc_offset_hr) {
  suppressMessages(data <- readr::read_csv(file, skip = 1))

  check_hobo_csv_data(data, file)

  data <- data[,1:5]

  meta <- extract_hobo_meta_data(data)

  colnames(data) <- c("FileRow", "DateTime", "Temperature", "CouplerDetached", "EndOfFile")

  data %<>% filter_hobo_data(file)

  data$FileName <- str_replace(basename(file), "[.]csv$", "")
  data$Directory <- dirname(file)

  data$Logger <- meta$Logger

  data$DateTime %<>% lubridate::parse_date_time(orders = orders, tz = "UTC")
  data$DateTime %<>% magrittr::add(lubridate::hm(meta$TimeZoneOffset))
  data$DateTime %<>% magrittr::subtract(utc_offset_hr)

  data$Temperature %<>% udunits2::ud.convert(meta$TempUnits, temp_units)

  data %<>% select_(~Logger, ~DateTime, ~Temperature, ~FileRow, ~FileName, ~Directory)

  colnames(data)[3] <- str_c("Temperature", temp_units, sep = "_")

  data %<>% as.tbl()
  data
}
#
# Loggerfunc <- function(Loggerfile) {
#
#   stopifnot(is.character(Loggerfile))
#
#   dat <- read_csv(Loggerfile, skip=1)
#
#
#   dat$TZHobo <- str_replace(colnames(dat)[2],".*(\\w+\\s\\w+[,]\\s)", "\\")
#
#
#   colnames(dat)[2] <- 'DateTime'
#   colnames(dat)[3] <- 'Temperature'
#   dat$Logger <- str_replace(colnames(dat)[5], ".*(\\sS[/]N[:]\\s)(\\d+)[)]$", "\\2")
#   dat %<>% select(DateTime, Temperature, Logger, TZHobo, FileName, FileDirectory)
#
#
#   dat$DateTime %<>% dmy_hms()
#
#   dat %<>% mutate(TempYear = year(DateTime))
#   dat %<>% mutate(TempMonth = month(DateTime))
#   dat %<>% mutate(TempDay = day(DateTime))
#   dat %<>% mutate(TempHour = hour(DateTime))
#   dat %<>% mutate(TempMinute = minute(DateTime))
#   dat %<>% mutate(TempSecond = second(DateTime))
#
#   dat$DateTime <- NULL
#
#   return(dat)
# }


#' Read Hobo CSVs
#'
#' @param file A string of the file or directory.
#' @param orders A character vector of date-time formats used by \code{\link[lubridate]{parse_date_time}}.
#' @param temp_units A string of the units to convert the temperature data to using  \code{\link[udunits2]{ud.convert}}.
#' @param utc_offset_hr A number of the UTC offset in hours (ie the returned date times are 'labelled' as UTC but are actually UTC - hours(utc_offset_hr).
#' @param recursive A flag indicating whether to read files from subdirectories.
#' Ignored if file is a file (as opposed to a directory).
#' @return A tibble of the data with the temperature.
#' @export
#' @examples
#' read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"))
read_hobo_csv <- function(file = ".", orders = c("Ymd HMS", "dmy HMS"),
                          temp_units = "degC", utc_offset_hr = -8, recursive = FALSE) {
  check_string(file)
  check_string(temp_units)
  check_number(utc_offset_hr)
  check_flag(recursive)

  if (str_detect(file, "[.]csv$")) {
    if (recursive) warning("recursive ignored as file is a single file")
    return(read_hobo_csv_file(file, orders, temp_units, utc_offset_hr))
  }
  files <- list.files(file, pattern = "[.]csv$", full.names = TRUE)
  if (!length(files)) return(dplyr::data_frame(x = character(0)))

  data <- lapply(files, read_hobo_csv_file, orders, temp_units, utc_offset_hr)
  data
}
