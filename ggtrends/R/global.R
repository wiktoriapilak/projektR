library(shiny)
library(DT)
library(ggplot2)
library(data.table)
race_cols = c('track_id','race_date','race_number','distance_id','course_type','track_condition','run_up_distance','race_type','purse','post_time')
race <- read.csv('C:/Users/user/Desktop/R/DERBY/nyra_race_table.csv', sep = ",")
race = as.data.table(race)
race$race_date =  as.Date(race$race_date, format = '%Y-%m-%d')
complete = readRDS('complete.rds')
start = readRDS("start.rds")

race_type = c("wszystkie",unique(complete$race_type))
jockey_type = sample(unique(complete$jockey),12)

wykres_kolowy_tor = readRDS("wykres_kolowy_tor.rds")
wykres_kolowy_miesiac = ggplot(complete, aes(x = "", y = ..count.., fill = as.factor(month))) +
  geom_bar(stat = "count", width = 1, color = "black") +
  geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size = 3)+
  coord_polar("y", start = 0) +
  theme_void() +
  labs( fill = "miesiąc")+
  scale_fill_brewer(palette = "Set3")

derby_race_types_histogram = ggplot(complete, aes(x = race_type, y = (..count..)/sum(..count..) * 100, fill= race_type)) +
  geom_bar(stat = "count", color = "white", position = "dodge") +
  geom_text(stat = "count", aes(label = paste0(round((..count..)/sum(..count..) * 100, 1), "%")),
            position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Typy wyścigów konnych", x = "Typ wyścigu", y = "Procentowy udział") +
  theme_bw()+
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))


dt_jockey <- merge(race[, c('race_type', 'race_number', 'race_date')],
                     start,
                     by = c('race_number', 'race_date'))
  
jockey_first_place <- dt_jockey[final_place == 1, .(sum = .N), by = .(jockey, race_type)]
  
g <- jockey_first_place[, .(total_sum = sum(sum)), by = jockey][order(-total_sum)]
ost_top_10 <- merge(g[1:10], jockey_first_place, by = "jockey")   
  
 
jockey_race_type <- jockey_first_place[race_type == race_type][order(-sum)]
  
#jockey_per_tor 


