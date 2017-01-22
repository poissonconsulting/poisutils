no_hobo_data <- function(tz) {
  datetime <- Sys.time()
  lubridate::tz(datetime) <- tz
  data <- dplyr::data_frame(Logger = "", DateTime = datetime,
                            Temperature = 1, FileRow = 1L,
                            FileName = "", Directory = "")
  data %<>% slice_(~0)
  data
}

rename_hobo_data <- function(data, units) {
  colnames(data)[3] <- str_c("Temperature_", units)
  data
}

check_hobo_csv_data_colname <- function(colnames, pattern, which, file) {
  if (!str_detect(colnames[which], pattern))
    error("Column '", colnames[which], "' in file '", file, "', does not match the regular expression '", pattern, "'")
}

check_hobo_csv_data_colnames <- function(data, file) {
  colnames <- colnames(data)
  check_hobo_csv_data_colname(colnames, "^#$", 1, file)
  check_hobo_csv_data_colname(colnames, "^Date Time, GMT", 2, file)
  check_hobo_csv_data_colname(colnames, "^Temp, ", 3, file)
  data
}

check_hobo_csv_data <- function(data, file) {
  check_hobo_csv_data_colnames(data, file)
  data
}

extract_hobo_meta_data_units <- function(colname) {
  unit <- str_extract(colname,  "(?<=Temp, )(.{1,3})(?= [(LGR])")[[1]]
  unit %<>% str_replace("[?][?]C", "degC")
  unit %<>% str_replace("[?][?]F", "degF")
  unit
}

extract_hobo_meta_data_logger <- function(colname) {
  str_extract(colname,  "(?<=LGR S[/]N[:] )(\\d+)(?=,|[)])")[[1]]
}

extract_hobo_meta_data_tz_offset <- function(colname) {
  str_extract(colname,  "(?<=^Date Time, GMT)([^\n]{2,})(?=$)")[[1]]
}

extract_hobo_meta_data <- function(colnames) {
  data_frame(Logger = extract_hobo_meta_data_logger(colnames[3]),
             TempUnits = extract_hobo_meta_data_units(colnames[3]),
             TimeZoneOffset = extract_hobo_meta_data_tz_offset(colnames[2]))
}

read_hobo_csv_file <- function(file, orders, units, tz, quiet) {
  suppressMessages(data <- readr::read_csv(file, skip = 1))

  check_hobo_csv_data(data, file)

  meta <- extract_hobo_meta_data(colnames(data))

  data <- data[,1:3]
  colnames(data) <- c("FileRow", "DateTime", "Temperature")
  data %<>% dplyr::filter_(~!is.na(Temperature))

  if (nrow(data)) {

    data$FileName <- str_replace(basename(file), "[.]csv$", "")
    data$Directory <- dirname(file)

    data$Logger <- meta$Logger

    data$DateTime %<>% lubridate::parse_date_time(orders = orders, tz = "UTC")

    data$DateTime %<>% magrittr::subtract(lubridate::hm(meta$TimeZoneOffset))
    data$DateTime %<>% lubridate::with_tz(tz)

    data$Temperature %<>% udunits2::ud.convert(meta$TempUnits, units)
    data %<>% select_(~Logger, ~DateTime, ~Temperature, ~FileRow, ~FileName, ~Directory)
  } else
    data <- no_hobo_data(tz)

  data %<>% rename_hobo_data(units)

  if (!quiet) message("imported ", nrow(data), " rows of data from '", file, "'")

  data %<>% as.tbl()
  data
}

#' Read Hobo CSVs
#'
#' @param file A string of the file or directory.
#' @param orders A character vector of date-time formats used by \code{\link[lubridate]{parse_date_time}}.
#' @param units A string of the units to convert the temperature data to using  \code{\link[udunits2]{ud.convert}}.
#' @param tz A string of the time zone to convert the date times to using \code{\link[lubridate]{with_tz}}.
#' @param recursive A flag indicating whether to read files from subdirectories.
#' @param quiet A flag indicating whether to provide messages.
#' Ignored if file is a file (as opposed to a directory).
#' @return A tibble of the data with the temperature.
#' @export
#' @examples
#' read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"))
read_hobo_csv <- function(file = ".", orders = c("Ymd HMS", "dmy HMS"),
                          units = "degC", tz = "Etc/GMT+8", recursive = FALSE,
                          quiet = FALSE) {
  check_string(file)
  check_string(units)
  check_string(tz)
  check_flag(recursive)
  check_flag(quiet)

  if (str_detect(file, "[.]csv$")) {
    if (recursive) warning("recursive ignored as file is a single file")
    return(read_hobo_csv_file(file, orders, units, tz, quiet))
  }
  files <- list.files(file, pattern = "[.]csv$", full.names = TRUE, recursive = recursive)
  if (!length(files)) {
    warning("no .csv files found")
    data <- no_hobo_data(tz)
    data %<>% rename_hobo_data(units, tz)
    return(data)
  }
  data <- lapply(files, read_hobo_csv_file, orders, units, tz, quiet)
  data %<>% bind_rows()
  data %<>% arrange_(~Logger)
  data
}
