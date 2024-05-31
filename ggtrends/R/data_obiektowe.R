
library(methods)
library(dplyr)
library(ggplot2)
library(tidyr)

FashionTrend <- setClass(
  "FashionTrend",
  slots = list(
    data = "list",
    combined_df = "data.frame"
  )
)

setGeneric("initializeFashionTrend", function(.Object, files) {
  standardGeneric("initializeFashionTrend")
})

setMethod("initializeFashionTrend", "FashionTrend", function(.Object, files) {
  .Object@data <- lapply(files, function(file) read.csv(file, header = FALSE))
  names(.Object@data) <- sapply(strsplit(files, "\\."), `[`, 1)
  .Object <- process_data(.Object)
  .Object
})

setGeneric("process_data", function(.Object) {
  standardGeneric("process_data")
})

setMethod("process_data", "FashionTrend", function(.Object) {
  for (name in names(.Object@data)) {
    .Object@data[[name]] <- .Object@data[[name]] %>%
      mutate(rodzaj = name) %>%
      slice(-1, -2)
  }

  .Object@combined_df <- bind_rows(.Object@data)
  names(.Object@combined_df) <- c("tydzien", "wartosc", "rodzaj")
  .Object@combined_df <- .Object@combined_df %>%
    rename(data = tydzien) %>%
    separate(data, into = c("rok", "miesiac", "dzien"), sep = "-") %>%
    mutate(
      wartosc = as.numeric(as.character(wartosc)),
      rok = as.numeric(rok)
    ) %>%
    filter(!is.na(wartosc))

  .Object@combined_df <- .Object@combined_df %>%
    mutate(przedzial_lat = cut(rok, breaks = seq(2003, 2024, by = 3),
                               labels = c("2004-2006", "2007-2009", "2010-2012", "2013-2015", "2016-2018", "2019-2021", "2022-2024"),
                               right = TRUE))
  .Object
})

setGeneric("plot_histogram", function(.Object) {
  standardGeneric("plot_histogram")
})

setMethod("plot_histogram", "FashionTrend", function(.Object) {
  ggplot(.Object@combined_df, aes(x = rok)) +
    geom_histogram(fill = "skyblue", color = "black", bins = 20) +
    labs(title = "Histogram of Years by Type",
         x = "Year",
         y = "Frequency") +
    theme_minimal() +
    facet_wrap(~ rodzaj, scales = "free")
})

setGeneric("plot_boxplot", function(.Object) {
  standardGeneric("plot_boxplot")
})

setMethod("plot_boxplot", "FashionTrend", function(.Object) {
  ggplot(.Object@combined_df, aes(x = przedzial_lat, y = wartosc, fill = rodzaj)) +
    geom_boxplot() +
    labs(title = "Rozkład wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
         x = "Przedział czasowy",
         y = "Wartość",
         fill = "Rodzaj") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

setGeneric("calculate_statistics", function(.Object) {
  standardGeneric("calculate_statistics")
})

setMethod("calculate_statistics", "FashionTrend", function(.Object) {
  statystyki_opisowe <- .Object@combined_df %>%
    group_by(przedzial_lat, rodzaj) %>%
    summarize(
      srednia = mean(wartosc, na.rm = TRUE),
      mediana = median(wartosc, na.rm = TRUE),
      odchylenie_standardowe = sd(wartosc, na.rm = TRUE),
      n = n()
    ) %>%
    ungroup()
  statystyki_opisowe
})

setGeneric("plot_statistics", function(.Object, stat_col, stat_name) {
  standardGeneric("plot_statistics")
})

setMethod("plot_statistics", "FashionTrend", function(.Object, stat_col, stat_name) {
  statistics_df <- calculate_statistics(.Object)
  ggplot(statistics_df, aes_string(x = "przedzial_lat", y = stat_col, color = "rodzaj", group = "rodzaj")) +
    geom_line() +
    geom_point() +
    labs(title = paste(stat_name, "wartości dla różnych rodzajów w poszczególnych przedziałach czasowych"),
         x = "Przedział czasowy",
         y = paste(stat_name, "wartość")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
})

# Użycie
files <- c("baggy-jeans.csv", "bandage-dress.csv", "bucket-hat.csv", "chloe-paddington.csv", 
           "chocker.csv", "emo-hair.csv", "fedora.csv", "flip-flop.csv", "printed-jeans.csv", 
           "ripped-jeans.csv", "sock-boots.csv", "tube-top.csv", "ugg-boots.csv")

fashion_trend <- FashionTrend()
fashion_trend <- initializeFashionTrend(fashion_trend, files)

# Wyświetlanie histogramu
print(plot_histogram(fashion_trend))

# Wyświetlanie boxplot
print(plot_boxplot(fashion_trend))

# Obliczanie i wyświetlanie statystyk opisowych
statystyki_opisowe <- calculate_statistics(fashion_trend)
print(statystyki_opisowe)

# Wyświetlanie wykresów statystyk
print(plot_statistics(fashion_trend, "srednia", "Średnie"))
print(plot_statistics(fashion_trend, "mediana", "Median"))
print(plot_statistics(fashion_trend, "odchylenie_standardowe", "Odchylenie standardowe"))


