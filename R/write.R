#' Write data file to .xlsx format
#'
#' @param data Dataframe
#' @param folder Output folder
#' @param split_by_station Boolean; If TRUE split by station
#' @param overwrite Boolean; FALSE by default.
#'
#' @returns This function is used for the side-effect of writing a file
#' @export
#'
write_data <- function(
  data,
  folder,
  split_by_station = TRUE,
  overwrite = FALSE
) {
  date_out <- unique(lubridate::date(data$datetime))
  if (length(date_out) > 1) {
    stop("Can't write multiple dates at once.")
  }
  datestring_out <- format(date_out, "%Y%m%d")

  # Convert datetimes to strings
  data$datetime <- format(data$datetime, "%Y-%m-%d %H:%M:%S", trim = TRUE)

  if (split_by_station) {
    file_out_s1 <- file.path(folder, paste0(datestring_out, "_S1.xlsx"))
    file_out_s2 <- file.path(folder, paste0(datestring_out, "_S2.xlsx"))

    if ((file.exists(file_out_s1) | file.exists(file_out_s2)) & !overwrite) {
      stop("Files already exist.")
    }

    station1 <- subset(data, telpost == "1. Sakhalvasho")
    station2 <- subset(data, telpost == "2. Shuamta")

    writexl::write_xlsx(station1, file_out_s1)
    writexl::write_xlsx(station2, file_out_s2)
  } else {
    file_out <- file.path(folder, paste0(datestring_out, ".xlsx"))

    if (file.exists(file_out) & !overwrite) {
      stop("File alreadys exists.")
    }
    writexl::write_xlsx(data, file_out)
  }
}
