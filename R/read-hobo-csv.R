check_hobo_csv_data_colname <- function(colnames, pattern, file) {
  if (sum(str_detect(colnames, pattern)) != 1)
    error("file '", file, "', does not have a single columm name matching the regular expression '", pattern, "'")
}

check_hobo_csv_data_colnames <- function(data, file) {
  colnames <- colnames(data)
  check_hobo_csv_data_colname(colnames, "^#$", file)
  check_hobo_csv_data_colname(colnames, "^Date Time,", file)
  check_hobo_csv_data_colname(colnames, "^Temp", file)
  check_hobo_csv_data_colname(colnames, "^Coupler Detached [(]LGR S/N:", file)
  check_hobo_csv_data_colname(colnames, "^End Of File [(]LGR S/N:", file)
  data
}

check_hobo_csv_data <- function(data, file) {
  check_hobo_csv_data_colnames(data, file)
  data
}

read_hobo_csv_file <- function(file) {
  data <- readr::read_csv(file, skip = 1)

  check_hobo_csv_data(data)

  # add checking colnames
  # also get temperature....
  #

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
#   dat$FileName <- str_replace(Loggerfile,".*([/])([\\w\\s[.]]+)([.]csv$)", "\\2")
#
#   dat$FileDirectory <- str_replace(Loggerfile,".*([/])([\\w\\s[.]]+)([/][\\w\\s[.]]+[.]csv$)", "\\2")
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
#' @param recursive A flag indicating whether to read files from subdirectories.
#' Ignore if dir is a file.
#' @return A tibble of the data.
#' @export
#' @examples
#' read_hobo_csv(system.file("hobo", "10723440.csv", package = "poisutils"))
read_hobo_csv <- function(file = ".", recursive = FALSE) {
  check_string(file)
  check_flag(recursive)

  if (str_detect(file, "[.]csv$")) {
    if (recursive) warning("recursive ignored as file as a single file")
    return(read_hobo_csv_file(file))
  }
  files <- list.files(file, pattern = "[.]csv$", full.names = TRUE)
  if (!length(files)) return(dplyr::data_frame(x = character(0)))

  data <- lapply(files, read_hobo_csv_file)
  data
}
