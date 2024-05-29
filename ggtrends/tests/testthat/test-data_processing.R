test_that("read_all_files function works properly", {
  folder_path <- "C:/Users/Wiktoria/Desktop/ProjektR/ggtrends/data"
  files_and_data <- read_all_files(folder_path)
  expect_true(is.list(files_and_data))
  expect_true("data_list" %in% names(files_and_data))
  expect_true("files" %in% names(files_and_data))
})

test_that("split_date function works properly", {
  data <- data.frame(V1 = c("2024-05", "2023-04", "2022-03"))
  data <- split_date(data)
  expect_true("year" %in% names(data))
  expect_true("month" %in% names(data))
})

test_that("add_year_range_column function works properly", {
  data <- data.frame(year = c(2004, 2008, 2012, 2015, 2019, 2023))
  data <- add_year_range_column(data)
  expect_true("range_of_years" %in% names(data))
})

test_that("add_season_column function works properly", {
  data <- data.frame(month = c(1, 4, 7, 10))
  data <- add_season_column(data)
  expect_true("season" %in% names(data))
})

test_that("combine_data_frames function works properly", {
  data_list <- list(data.frame(value = 1:3, type = "type1"),
                    data.frame(value = 4:6, type = "type2"))

  combined_df <- combine_data_frames(data_list)
  expect_equal(nrow(combined_df), sum(sapply(data_list, nrow)))
  expect_equal(ncol(combined_df), ncol(data_list[[1]]))
})
