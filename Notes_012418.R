# select is for columns
# filter is for rows


# install.packages("tidyverse")
library(tidyverse)
library(nycflights13)
flights
# I think the %>% loads the dataset so that I have access to it without
# constantly having to refer to it
flights %>%
  filter(month == 1, day == 1)
# filter(flights, month = 1 )

(Jan1 <- filter(flights, month ==1 & day == 1))
Dec25 <- filter(flights, month == 12 & day == 25)
names(Dec25)

NovDec <- filter(flights, month == 11 | month == 12)
NovDec <- filter(flights, month %in% c(11,12))

NoNovDec <- filter (flights, (month ==11 | month == 12))
NoNoveDec <- filter (flights, month %in% c(11,12))

# explanation point is the not operator
filter(flights, !(arr_delay >120 | dep_delay >120))

filter(flights, arr_delay <= 120 & dep_delay <= 120)

# dollar sign lets us pick a column, then the first cell in that column is [1]
Dec25$arr_time[1] = NA

as.tbl(data.frame(x = c(1, NA, 3)))

# Find all flights that
# 
# Had an arrival delay of two or more hours

filter(flights, arr_delay >= 120)

# Flew to Houston (IAH or HOU)
filter(flights, dest %in% c('IAH', 'HOU'))

# Were operated by United, American, or Delta
filter(flights, carrier %in% c('UA', 'DL', 'AA'))

# Departed in summer (July, August, and September)
filter(flights, month %in% c(7,8,9))

# Arrived more than two hours late, but didn’t leave late
filter(flights, arr_delay >= 120 & dep_delay <= 0)

# Were delayed by at least an hour, but made up over 30 minutes in flight
filter(flights, dep_delay >= 60 & arr_delay <= 30)

# Departed between midnight and 6am (inclusive)
filter(flights, dep_time >= 0 & dep_time <= 6)
# Another useful dplyr filtering helper is between(). What does it do? Can you use it to simplify the code needed to answer the previous challenges?
filter(flights, between(dep_time, 0, 600))
#   
#   How many flights have a missing dep_time? What other variables are missing? What might these rows represent?
filter(flights, is.na(dep_time))
#   
#   Why is NA ^ 0 not missing? Why is NA | TRUE not missing? Why is FALSE & NA not missing? Can you figure out the general rule? (NA * 0 is a tricky counterexample!)

arrange(flights, year, month, day)

arrange(flights, desc(arr_delay))
arrange(df, is.na(x))


select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year: arr_time))

select(flights, start_with('arr_'))
select(flights, ends_with('_delay'))

select(flights, matches("\\_"))

# get list of names of columns
names(flights)

select(flights
       ,time_hour
       ,everything()
       )

select(flights, year, year)

select(flights, contains("TIME"))

# %>% operator chains commands together
# mutate allows you to modify the data set
# note that gain is created in the mutate function and then is also used
# inside the function
mutate(flights
       ,gain = arr_delay - dep_delay
       ,hours = air_time/60
       ,gain_per_hour = gain/hours
       ) %>%
head(1000) %>%
  View

#similar to mutate, except it only keeps the data columns you create
transmute(flights
       ,gain = arr_delay - dep_delay
       ,hours = air_time/60
       ,gain_per_hour = gain/hours
) %>%
  head(1000) %>%
  View


delay<-
flights%>%
  #filter(!is.na(delay))%>%
  group_by(year, month, day) %>%
  summarise(
    count = n()
    , delay = mean(arr_delay, na.rm = TRUE)
    , dist = mean(distance, na.rm = TRUE)
  )


ggplot(delay, aes(x = dist, y = delay)) + 
  geom_point(aes(size = count), alpha = .3) + # sizes dots based on number of #counts
# alpha parameter makes the dots more transparent, darker areas mean more dots
geom_smooth(se = FALSE) + 
  #theme_bw()
  theme_classic()

flights%>%
  #filter(!is.na(delay))%>%
  group_by(year, month, day) %>%
  summarise(
    count = n()
    , delay = mean(arr_delay, na.rm = TRUE)
    , dist = mean(distance, na.rm = TRUE)
  )
filter(delay > 35)


flights %>%
  group_by(carrier) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  mutate(
    Percentage = round(100*count/sum(count), 3)
  )%>%
  arrange(desc(Percentage)) %>%
   left_join(airlines)

# left_join(airlines, by = c('carrier' = 'InsertCarrierNameHere'))

flights %>%
  group_by(year, month, day) %>%
  summarise(
    count = n()
  ) %>%
  ungroup() %>%
  mutate(
    Percentage = round(100*count/sum(count), 3)
    ,sum(count)
  )%>%
  arrange(desc(Percentage))





















