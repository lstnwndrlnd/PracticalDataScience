# Author: Eddie Polanco
# Homework #3
library(tidyverse)
wineData <- read.csv(file="/Users/eddie/Documents/PracticalDataScience/data/winemag-data-130k-v2.csv",head=TRUE,sep=",")

# print out class: wineData is a dataframe 
class(wineData)

# remove 1st column (named X) from dataset
Main <-
  select(wineData, -X)

# filter dataset down to only wines from France
frenchWines <-
  filter(Main, country == 'France')

frenchWines %>%
  summarise(
    count = n()
    ,min_price = min(price, na.rm = TRUE)
    ,max_price = max(price, na.rm = TRUE)
    ,mean_price = mean(price, na.rm = TRUE)
)

frenchWines %>%
  group_by(taster_name)%>%
  summarise(
    count = n()
    ,min_price = min(price, na.rm = TRUE)
    ,max_price = max(price, na.rm = TRUE)
    ,mean_price = mean(price, na.rm = TRUE)
  )%>%
  arrange(desc(count))

frenchWines %>%
  group_by(country)%>%
  summarise(
    count = n()
    ,min_price = min(price, na.rm = TRUE)
    ,max_price = max(price, na.rm = TRUE)
    ,mean_price = mean(price, na.rm = TRUE)
  )%>%
  arrange(desc(count))

frenchWines %>%
  drop_na()%>%
  group_by(variety)%>%
  summarise(
    count = n()
    ,min_price = min(price, na.rm = TRUE)
    ,max_price = max(price, na.rm = TRUE)
    ,mean_price = mean(price, na.rm = TRUE)
  )%>%
  arrange(desc(count))

Main%>%
  filter(grepl('tannins', description) | grepl('finish', description))%>%
  group_by(variety)%>%
  summarise(
    count = n()
  )%>%
  head(15)%>%
    arrange(desc(count))



Features1 <-
  Main%>%
  group_by(variety)%>%
  summarise(
    counts = n()
    ,tannins = mean(grepl('tannins', description))
    ,finish = mean(grepl('finish', description))
    ,cherry = mean(grepl('cherry', description))
    ,plum = mean(grepl('plum', description))
  )%>%
  filter(counts > 150)%>%
  arrange(desc(tannins))%>%
  head(5)

Features2 <-
  Main%>%
  group_by(variety)%>%
  summarise(
    counts = n()
    ,tannins = mean(grepl('tannins', description))
    ,finish = mean(grepl('finish', description))
    ,cherry = mean(grepl('cherry', description))
    ,plum = mean(grepl('plum', description))
  )%>%
  filter(counts > 150)%>%
  arrange(desc(finish))%>%
  head(5)
  
Features3 <-
  Main%>%
  group_by(variety)%>%
  summarise(
    counts = n()
    ,tannins = mean(grepl('tannins', description))
    ,finish = mean(grepl('finish', description))
    ,cherry = mean(grepl('cherry', description))
    ,plum = mean(grepl('plum', description))
  )%>%
  filter(counts > 150)%>%
  arrange(desc(cherry))%>%
  head(5)

Features4 <-
  Main%>%
  group_by(variety)%>%
  summarise(
    counts = n()
    ,tannins = mean(grepl('tannins', description))
    ,finish = mean(grepl('finish', description))
    ,cherry = mean(grepl('cherry', description))
    ,plum = mean(grepl('plum', description))
  )%>%
  filter(counts > 150)%>%
  arrange(desc(plum))%>%
  head(5)

Features <-
  rbind(Features1, Features2, Features3, Features4)
  
Main%>%
  group_by(taster_name)%>%
  summarise(
    counts = n()
  )%>%
  mutate(proportion = counts/nrow(Main))%>%
  select(taster_name, proportion)%>%
  arrange(desc(proportion))
  
  
  


