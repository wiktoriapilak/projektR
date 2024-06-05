
#' Load all CSV files from the specified folder
#'
#' @param folder_path The path to the folder containing CSV files.
#' @return A list containing two lists: `data_list` with loaded data and `files` with filenames.
#' @importFrom utils list.files
#' @export
read_all_files <- function(folder_path) {
  files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  data_list <- lapply(files, read.csv, header = FALSE)
  return(list(data_list = data_list, files = files))
}


#' Split date into month and year
#'
#' @param data A data frame containing a date column.
#' @return The modified data frame with `year` and `month` columns added.
#' @export
split_date <- function(data) {
  data$year <- as.integer(substr(data$V1, 1, 4))
  data$month <- as.integer(substr(data$V1, 6, 7))
  return(data)
}


#' Add a column for year ranges
#'
#' @param data A data frame containing a `year` column.
#' @return The modified data frame with `range_of_years` column added.
#' @export
add_year_range_column <- function(data) {
  data$range_of_years <- cut(
    data$year,
    breaks = seq(2003, 2024, by = 3),
    labels = c("2004-2006", "2007-2009", "2010-2012", "2013-2015", "2016-2018", "2019-2021", "2022-2024"),
    right = TRUE
  )
  return(data)
}

#' Add a column for the season
#'
#' @param data A data frame containing a `month` column.
#' @return The modified data frame with `season` column added.
#' @export
add_season_column <- function(data) {
  data$season <- cut(
    data$month,
    breaks = c(0, 3, 6, 9, 12),
    labels = c("Winter", "Spring", "Summer", "Autumn"),
    right = TRUE,
    include.lowest = TRUE
  )
  return(data)
}

#' Process a list of data frames
#'
#' @param data_list A list of data frames.
#' @return A list of processed data frames.
#' @export
process_df <- function(data_list) {
  processed_df <- lapply(data_list, function(data) {
    rodzaj_value <- gsub(":.*", "", data[2, 2])
    data$type <- rodzaj_value
    data <- data[-c(1:2), ]
    data <- split_date(data)
    data <- add_year_range_column(data)
    data <- add_season_column(data)
    colnames(data)[1] <- "date"
    colnames(data)[2] <- "value"
    data$value <- as.numeric(data$value)
    data$type <- rodzaj_value
    return(data)
  })
  return(processed_df)
}

#' Combine a list of data frames into one
#'
#' @param processed_df A list of processed data frames.
#' @return A combined data frame.
#' @importFrom dplyr bind_rows
#' @export
combine_data_frames <- function(processed_df) {
  combined_df <- bind_rows(processed_df)
  return(combined_df)
}
