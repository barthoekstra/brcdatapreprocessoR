#' Process data from today
#'
#' A wrapper function around `process_day` that defaults to today
#'
#' @param tt_file Path to Trektellen .csv file
#' @param output_folder Path to output directory where raw/, inprogress/,
#' inprogress-backup/ and clean/ folders sit
#'
#' @returns This function is used for the side-effect of processing today's data
#' @export
process_today <- function(tt_file, output_folder) {
  today <- format(Sys.Date(), "%Y-%m-%d")
  process_day(day_str = today, tt_file, output_folder)
}

#' Process data from a day
#'
#' @param day_str Date string in YYYY-MM-DD format
#' @param tt_file Path to Trektellen .csv file
#' @param output_folder Path to output directory where raw/, inprogress/,
#' inprogress-backup/ and clean/ folders sit
#'
#' @returns This function is used for the side-effect of processing a day's data
#' @export
process_day <- function(day_str, tt_file, output_folder) {
  data <- preprocess_raw_trektellen_data(csv_path = tt_file, date_str = day_str)
  write_data(data, file.path(output_folder, "raw/"))
  data <- check_trektellen_data(data)
  write_data(data, file.path(output_folder, "inprogress/"))
  write_data(data, file.path(output_folder, "inprogress-backup/"))
  message(paste0("Finished processing files for", day_str))
}
