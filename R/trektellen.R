#' Extract count period from Trektellen website
#'
#' @param station Station ID or name
#' @param date_str Date string
#'
#' @returns NULL if failed, otherwise a list with start and end times
#' @export
extract_count_period <- function(station, date_str) {
  if (is.numeric(station)) {
    if (!station %in% c(1047, 1048)) {
      stop("Station ID should be 1047 or 1048")
    }
    station_id <- station
  } else if (is.character(station)) {
    if (!station %in% c("1. Sakhalvasho", "2. Shuamta")) {
      stop("Station name should be one of \"1. Sakhalvasho\" or \"2. Shuamta\"")
    }
    # Set station ID
    if (station == "1. Sakhalvasho") {
      station_id <- 1047
    }
    if (station == "2. Shuamta") {
      station_id <- 1048
    }
  } else {
    stop("station should be the station ID or a name")
  }

  tt_url <- sprintf(
    "https://trektellen.org/count/view/%d/%s?sc=0",
    station_id,
    gsub("-", "", date_str)
  )

  tt_count_page <- rvest::read_html(tt_url)

  tt_count_info <- rvest::html_element(tt_count_page, "#left_content table tr")
  if (is.na(tt_count_info)) {
    message("Fetching count times from Trektellen failed, returning NULL")
    return(NULL)
  }
  tt_count_text <- rvest::html_text2(tt_count_info)
  tt_count_periods <- strsplit(tt_count_text, split = "\n")[[1]][1]

  tt_count_periods <- gsub("Counting period: ", "", tt_count_periods)
  tt_count_periods <- trimws(strsplit(tt_count_periods, split = "&")[[1]])

  if (length(tt_count_periods) == 1) {
    tt_count_periods <- strsplit(tt_count_periods, "-")[[1]]
    tt_count_start <- hms::parse_hm(tt_count_periods[[1]])
    tt_count_end <- hms::parse_hm(tt_count_periods[[2]])
    tt_breakresumes <- NULL
  } else {
    tt_count_periods <- unlist(strsplit(tt_count_periods, "-"))
    tt_count_start <- hms::parse_hm(tt_count_periods[1])
    tt_count_end <- hms::parse_hm(tt_count_periods[length(tt_count_periods)])
    tt_breakresumes <- tt_count_periods[c(2:(length(tt_count_periods) - 1))]
    tt_breakresumes <- lapply(tt_breakresumes, hms::parse_hm)
  }

  return(list(
    start = tt_count_start,
    end = tt_count_end,
    breakresumes = tt_breakresumes
  ))
}
