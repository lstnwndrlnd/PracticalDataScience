library(tidyverse)

ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point()

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))

ggplot(mpg, aes(x = hwy, y = cyl)) +
  geom_point()

ggplot(mpg, aes(class, drv)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(color = 'blue', size = pi, shape = 11)
# aes is a mapping, the color and size are not mappings when used in this way

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = 'blue'))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = ''))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = manufacturer, size = cyl))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = manufacturer, stroke = cyl))

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = displ > 5 & hwy < 20), size = .5)

ggplot(mpg, aes(hwy, cty)) +
  geom_point()

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  facet_wrap(~class, ncol = 2)

ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  facet_grid(drv~cyl)
# facet grid is really good for being able to cross reference different aspects of the data set
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class)) +
  facet_grid(.~ cyl)

# alpha = .4 makes the points transparent, darker points means multiple points
# position = 'jitter' makes it so that it expands multiple points so they can be seen, it messes up the scatter plot but is useful for binned or discreet data
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point(aes(color = class), alpha = .4, position = 'jitter') +
  geom_smooth(method = 'loess', span = 1, se = FALSE, color = 'maroon', alpha = 1) +
  # theme(axis.title.x=element_blank()
  #       ,axis.text.x=element_blank()
  #       ,axis.ticks.x=element_blank()
  #       ,axis.title.y=element_blank()
  #       ,axis.text.y=element_blank()
  #       ,axis.ticks.y=element_blank()
  #       )
  ylim(c(10,25))+
  theme_wsj()


ggplot(mpg, aes(x = displ, y = hwy, color = class)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~class)

ggplot(diamonds) +
  geom_bar(aes(x = cut))

dd<-diamonds%>%
  group_by(cut)%>%
  summarise(
    count = n()
  )

ggplot(dd, aes(x = cut, y = count))+
    geom_bar(stat = 'identity')

ggplot(diamonds, aes(x = cut, y = price))+
  geom_point(position = 'jitter', alpha = .5)

ggplot(diamonds, aes(x = cut, y = price))+
  geom_boxplot()

ggplot(diamonds, aes(x = cut, y = price)) +
  geom_violin(fill = 'black')




