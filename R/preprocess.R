#' Preprocess RAW Trektellen CSV to the standardised BRC form
#'
#' @param csv_path Path to .csv file with RAW Trektellen counts
#' @param date_str Date string in YYYY-MM-DD format to filter data from RAW counts
#'
#' @returns a pre-processed dataframe which follows BRC data formatting
#' @export
#'
preprocess_raw_trektellen_data <- function(csv_path, date_str = NULL) {
  # Load csv file
  data <- readr::read_csv(
    csv_path,
    col_types = readr::cols(
      date = readr::col_date(format = ""),
      timestamp = readr::col_time(format = ""),
      countid = readr::col_double(),
      telpost = readr::col_double(),
      speciesname = readr::col_character(),
      speciesid = readr::col_double(),
      count = readr::col_double(),
      countback = readr::col_double(),
      local = readr::col_double(),
      age = readr::col_character(),
      sex = readr::col_character(),
      plumage = readr::col_character(),
      remark = readr::col_character(),
      location = readr::col_character(),
      migtype = readr::col_character(),
      counttype = readr::col_character(),
      year = readr::col_double(),
      yday = readr::col_double()
    )
  )

  # Filter for a specific date
  if (!is.null(date_str)) {
    date_Date <- as.Date(date_str)

    on_date <- data$date == date_Date
    data <- data[on_date, ]

    if (nrow(data) == 0) {
      stop("Date selection resulted in 0 rows")
    }

    # Add dummy start and end times
    times <- data.frame(
      date = rep(as.Date(date_str), 4),
      timestamp = hms::hms(
        hours = c(0, 0, 23, 23),
        minutes = c(0, 0, 59, 59),
        seconds = c(0, 0, 59, 59)
      ),
      telpost = c(1047, 1048, 1047, 1048),
      speciesname = c("START", "START", "END", "END"),
      count = 1
    )
    data <- dplyr::bind_rows(times[1:2, ], data, times[3:4, ])
  }

  # Change timestamp to 00:00:00 if timestamp was missing
  timestamp_missing <- is.na(data$timestamp)
  data$timestamp[timestamp_missing] <- hms::as_hms(0)

  # Make datetime column
  data$datetime <- lubridate::ymd(data$date) + lubridate::hms(data$timestamp)

  # Remove unused columns, including the date and timestamp columns, which we can regenerate later on
  drop_cols <- c("date", "timestamp", "countid", "speciesid", "year", "yday")
  data <- data[, -which(names(data) %in% drop_cols)]

  # Reorder columns
  column_order <- c(
    "datetime",
    "telpost",
    "speciesname",
    "count",
    "countback",
    "local",
    "age",
    "sex",
    "plumage",
    "remark",
    "location",
    "migtype",
    "counttype"
  )
  data <- data[, column_order]

  # Replace station IDs with names
  data$telpost[data$telpost == 1047] <- "1. Sakhalvasho"
  data$telpost[data$telpost == 1048] <- "2. Shuamta"

  # Replace species names
  repl <- list(
    HB_AD = "HB_NONJUV",
    `large FALCON` = "Large FALCON",
    `Raptor-SPEC` = "Raptor_SPEC",
    `Stork-SPEC` = "Stork_SPEC",
    `Buzzard-SPEC` = "Buzzard_SPEC",
    `dove (Columba) sp.` = "Dove_SPEC",
    `Harrier-SPEC` = "Harrier_SPEC",
    `Oriental Turtle-Dove` = "OrientalTD",
    WhitePel = "WhiteP",
    DalPel = "DalmatianP"
  )
  for (k in names(repl)) {
    data$speciesname[data$speciesname == k] <- repl[[k]]
  }

  # Sort by datetime, telpost
  data <- data[order(data$datetime, data$telpost), ]

  data
}
