library(dplyr)
library(tools)

read_all_files <- function(folder_path) {
  files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  data_list <- lapply(files, read.csv, header = FALSE)
  return(list(data_list = data_list, files = files))
}

process_data <- function(data_list, files) {
  processed_list <- mapply(function(df, file) {
    type_value <- file_path_sans_ext(basename(file))
    df <- df %>%
      slice(-1, -2) %>%
      mutate(
        type = type_value,
        week = as.Date(V1, format="%Y-%m-%d"),
        year = as.numeric(format(week, "%Y")),
        month = format(week, "%m"),
        day = format(week, "%d"),
        range_of_years = cut(
          year,
          breaks = seq(2003, 2024, by = 3),
          labels = c("2004-2006", "2007-2009", "2010-2012", "2013-2015", "2016-2018", "2019-2021", "2022-2024"),
          right = TRUE
        )
      ) %>%
      select(-V1)
    return(df)
  }, data_list, files, SIMPLIFY = FALSE)

  return(processed_list)
}

combine_data_frames <- function(processed_list) {
  combined_df <- bind_rows(processed_list)
  names(combined_df) <- c("value", "type", "week", "year", "month", "day", "range_of_years")
  return(combined_df)
}
