library(dplyr)
library(tools)

read_all_files <- function(folder_path) {
  files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  data_list <- lapply(files, read.csv, header = FALSE)
  return(list(data_list = data_list, files = files))
}

split_date <- function(data) {
  data$year <- as.integer(substr(data$V1, 1, 4))
  data$month <- as.integer(substr(data$V1, 6, 7))
  data <- data[, -1]
  return(data)
}

add_year_range_column <- function(data) {
  data$range_of_years <- cut(
    data$year,
    breaks = seq(2003, 2024, by = 3),
    labels = c("2004-2006", "2007-2009", "2010-2012", "2013-2015", "2016-2018", "2019-2021", "2022-2024"),
    right = TRUE,
  )
  return(data)
}

add_season_column <- function(data) {
  data$season <- cut(
    data$month,
    breaks = c(0, 3, 6, 9, 12),
    labels = c("Spring", "Summer", "Autumn", "Winter"),
    right = TRUE,
    include.lowest = TRUE
  )
  return(data)
}

process_df <- function(data_list) {
  processed_df <- lapply(data_list, function(data) {
    rodzaj_value <- gsub(":.*", "", data[1, 2])
    data$type <- rodzaj_value
    data <- data[-c(1:2), ]
    data <- split_date(data)
    data <- add_year_range_column(data)
    data <- add_season_column(data)
    colnames(data)[1] <- "value"
    data$type <- rodzaj_value
    return(data)
  })
  return(processed_df)
}

combine_data_frames <- function(processed_df) {
  combined_df <- bind_rows(processed_df)
  return(combined_df)
}

#przykladowe uzycie
folder_path <- "C:/Users/Wiktoria/Desktop/ProjektR/ggtrends/data"
all_data <- read_all_files(folder_path)
processed_data <- process_df(all_data$data_list)
combined_df = combine_data_frames(processed_data)
