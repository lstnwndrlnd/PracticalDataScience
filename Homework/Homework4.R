#Author: Eddie Polanco
#Homework 4
#







  # 1. Using the tidyverse read in the wine dataset that can be downloaded from our Github repo. Read in the file and remove the X1 column again.
  # 
  # https://github.com/JackStat/PracticalDataScience/blob/master/data/winemag-data-130k-v2.csv (Links to an external site.)Links to an external site.
  # 
  # If you do not download it make sure you click "view raw" and use that url.
  # 
  wineData <- read.csv(file="/Users/eddie/Documents/PracticalDataScience/data/winemag-data-130k-v2.csv",head=TRUE,sep=",")
  
  # 
  # 2. Create a new dataset called Merlot and filter it down to only include the Merlot variety.
  # print out class: wineData is a dataframe 
  class(wineData)
  
  # remove 1st column (named X) from dataset
  Main <-
    select(wineData, -X)
  Merlot<-
    Main%>%
    filter(variety == 'Merlot', !is.na(price), !is.na(points))
  
  # a. Create a scatterplot with the price on y and points on x
ggplot(Merlot, aes(x = points, y = price)) +
    geom_point()
  # b. Create a boxplot with the price on y and points on x (tip: in your aesthetics add group = points)
ggplot(Merlot, aes(x = points, y = price, group = points)) +
  geom_boxplot()
  # c. Create a violin plot with the price on y and points on x (tip: also use group=points)
ggplot(Merlot, aes(x = points, y = price, group = points)) +
  geom_violin()  
  # 3. Create a new dataset called Top6 that includes the top 6 most commonly review wines varieties (most observations)
Top6Wines <- 
  Main%>%
  filter(!is.na(points), !is.na(price))%>%
  group_by(variety)%>%
  summarise(
    count = n()
  )%>%
  arrange(desc(count))%>%
  .[1:6,]
#  View()
# last line keeps the top six rows and all the columns
Top6 <-
  Main%>%
  inner_join(Top6Wines)%>%
  filter(!is.na(points), !is.na(price))

#or this line works too
#  filter(variety %in% Top6Wines)


# a. Create a scatterplot between price and points with colored points for each variety (Create this and then create a second one that adds the following layers)
ggplot(Top6, aes(x = points, y = price)) +
  geom_point(position = 'jitter', alpha = .25) +
  theme_wsj() + ylim(c(0,500))
  # 1. add jitter to the points so they don't overlap
ggplot(Top6, aes(x = points, y = price)) +
  geom_point(position = 'jitter')
  # 2. reduce the alpha level of the points to .25
ggplot(Top6, aes(x = points, y = price)) +
  geom_point(position = 'jitter', alpha = .25)
  # 3. add a theme that you like (for a list look at the help page for themes using ?theme_void in the console)
ggplot(Top6, aes(x = points, y = price)) +
  geom_point(position = 'jitter', alpha = .25) +
  theme_wsj() 
  # 4. add a limit on the y-axis to restrict the price between 0 and 500 (tip: ylim)
ggplot(Top6, aes(x = points, y = price)) +
  geom_point(position = 'jitter', alpha = .25) +
  theme_wsj() + ylim(c(0,500))
  # b. Create a scatterplot between price and points and use facets to create a separate plot for each variety  (Create this and then create a second one that adds the following layers)
ggplot(Top6, aes(x = points, y = price)) +
  geom_point() +
  facet_wrap(~variety)
  # 1. add theme_bw for the theme layer
ggplot(Top6, aes(x = points, y = price)) +
  geom_point() +
  facet_wrap(~variety)+
  theme_bw()
  # 2. add a smoothed line without a standard error (tip: se = FALSE)
ggplot(Top6, aes(x = points, y = price)) +
  geom_point() +
  facet_wrap(~variety)+
  theme_bw()+
  geom_smooth(se = FALSE)
  # 3. limit the y-axis to price between 0, 500
ggplot(Top6, aes(x = points, y = price)) +
  geom_point() +
  facet_wrap(~variety)+
  theme_bw()+
  geom_smooth(se = FALSE)+
  ylim(c(0,500))
  # c. Create a bar chart that shows the variety on x and the count on y (Create this and then create a second one that adds the following layers)
ggplot(Top6, aes(x = variety)) +
  geom_bar()
  # 1. facet_wrap the taster name
ggplot(Top6, aes(x = points)) +
  geom_bar()+
  facet_wrap(~taster_name)
  # 2. Now the x label looks like garbage so we need to create a legend for the variety. Do this by adding fill = variety to the aesthetics in geom_bar.
ggplot(Top6, aes(x = points)) +
  geom_bar(aes(fill = variety))+
  facet_wrap(~taster_name)
  # 3. Remove the x-axis labels and ticks (tip: use StackOverflow https://stackoverflow.com/questions/35090883/remove-all-of-x-axis-labels-in-ggplot (Links to an external site.)Links to an external site.)
ggplot(Top6, aes(x = points)) +
  geom_bar(aes(fill = variety))+
  facet_wrap(~taster_name)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  # 4. add a better-looking theme (Make sure you do this above your theme that gets rid of the x-axis labels because it will be overwritten if you don't)
ggplot(Top6, aes(x = points)) +
  geom_bar(aes(fill = variety))+
  facet_wrap(~taster_name)+
  theme_wsj()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  # 
  # 
