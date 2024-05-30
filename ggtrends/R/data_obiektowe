library(data.table)
library(tidyverse)
############ PRZYKŁADOWA KLASA, MOŻEMY JĄ TROCHĘ OGRANICZYĆ ################
# Definiowanie klasy FashionTrend
FashionTrend <- R6::R6Class("FashionTrend",
  public = list(
    data = NULL,
    combined_df = NULL,
    
    initialize = function(files) {
      self$data <- lapply(files, function(file) read.csv(file, header = FALSE))
      names(self$data) <- sapply(strsplit(files, "\\."), `[`, 1)
      self$process_data()
    },
    
    process_data = function() {
      for (name in names(self$data)) {
        self$data[[name]] <- self$data[[name]] %>%
          mutate(rodzaj = name) %>%
          slice(-1, -2)
      }
      
      self$combined_df <- bind_rows(self$data)
      names(self$combined_df) <- c("tydzien", "wartosc", "rodzaj")
      self$combined_df <- self$combined_df %>%
        rename(data = tydzien) %>%
        separate(data, into = c("rok", "miesiac", "dzien"), sep = "-") %>%
        mutate(
          wartosc = as.numeric(as.character(wartosc)),
          rok = as.numeric(rok)
        ) %>%
        filter(!is.na(wartosc))
      
      self$combined_df <- self$combined_df %>%
        mutate(przedzial_lat = cut(rok, breaks = seq(2003, 2024, by = 3),
                                   labels = c("2004-2006", "2007-2009", "2010-2012", "2013-2015", "2016-2018", "2019-2021", "2022-2024"),
                                   right = TRUE))
    },
    
    plot_histogram = function() {
      ggplot(self$combined_df, aes(x = rok)) +
        geom_histogram(fill = "skyblue", color = "black", bins = 20) +
        labs(title = "Histogram of Years by Type",
             x = "Year",
             y = "Frequency") +
        theme_minimal() +
        facet_wrap(~ rodzaj, scales = "free")
    },
    
    plot_boxplot = function() {
      ggplot(self$combined_df, aes(x = przedzial_lat, y = wartosc, fill = rodzaj)) +
        geom_boxplot() +
        labs(title = "Rozkład wartości dla różnych rodzajów w poszczególnych przedziałach czasowych",
             x = "Przedział czasowy",
             y = "Wartość",
             fill = "Rodzaj") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    },
    
    calculate_statistics = function() {
      statystyki_opisowe <- self$combined_df %>%
        group_by(przedzial_lat, rodzaj) %>%
        summarize(
          srednia = mean(wartosc, na.rm = TRUE),
          mediana = median(wartosc, na.rm = TRUE),
          odchylenie_standardowe = sd(wartosc, na.rm = TRUE),
          n = n()
        ) %>%
        ungroup()
      
      return(statystyki_opisowe)
    },
    
    plot_statistics = function(stat_col, stat_name) {
      statistics_df <- self$calculate_statistics()
      ggplot(statistics_df, aes_string(x = "przedzial_lat", y = stat_col, color = "rodzaj", group = "rodzaj")) +
        geom_line() +
        geom_point() +
        labs(title = paste(stat_name, "wartości dla różnych rodzajów w poszczególnych przedziałach czasowych"),
             x = "Przedział czasowy",
             y = paste(stat_name, "wartość")) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
  )
)


  files <- c("baggy-jeans.csv", "bandage-dress.csv", "bucket-hat.csv", "chloe-paddington.csv", 
           "chocker.csv", "emo-hair.csv", "fedora.csv", "flip-flop.csv", "printed-jeans.csv", 
           "ripped-jeans.csv", "sock-boots.csv", "tube-top.csv", "ugg-boots.csv")

fashion_trend <- FashionTrend$new(files)


  # Generowanie histogramu
fashion_trend$plot_histogram()

# Generowanie wykresu pudełkowego
fashion_trend$plot_boxplot()

# Obliczanie i wyświetlanie statystyk opisowych
statystyki_opisowe <- fashion_trend$calculate_statistics()
print(statystyki_opisowe)

# Generowanie wykresów statystyk
fashion_trend$plot_statistics("srednia", "Średnie")
fashion_trend$plot_statistics("mediana", "Median")
fashion_trend$plot_statistics("odchylenie_standardowe", "Odchylenie standardowe")

