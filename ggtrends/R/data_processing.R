library(dplyr)
library(tools)

read_all_files <- function(folder_path) {
  files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  data_list <- lapply(files, read.csv, header = FALSE)
  return(list(data_list = data_list, files = files))
}

process_data <- function(data_list, files) {
  processed_list <- mapply(function(df, file) {
    rodzaj_value <- file_path_sans_ext(basename(file))
    df <- df %>%
      slice(-1, -2) %>%
      mutate(
        value = rodzaj_value,
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
      mutate(month = as.numeric(month)) %>%
      mutate(
        season = case_when(
          month %in% c(12, 1, 2) ~ "Zima",
          month %in% c(3, 4, 5) ~ "Wiosna",
          month %in% c(6, 7, 8) ~ "Lato",
          month %in% c(9, 10, 11) ~ "Jesień"
        )
      ) %>%
      select(-V1)

    return(df)
  }, data_list, files, SIMPLIFY = FALSE)

  return(processed_list)
}


combine_data_frames <- function(processed_list) {
  combined_df <- bind_rows(processed_list)
  names(combined_df) <- c("value", "type", "week", "year", "month", "day", "range_of_years","season")
  return(combined_df)
}


#przykladowe uzycie
folder_path <- "C:/Users/Wiktoria/Desktop/ProjektR/ggtrends/data"
all_data <- read_all_files(folder_path)
processed_data <- process_data(all_data$data_list, all_data$files)
combined_df = combine_data_frames(processed_data)
