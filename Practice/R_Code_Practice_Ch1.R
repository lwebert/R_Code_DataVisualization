####################### Chapter 1 ##############################################

library(tidyverse)
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy))

################################## Aesthetic Mappings ###
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color= "blue"))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, alpha=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, shape=class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, size=class))

############################################### Facets ###
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_wrap(~class, nrow=2)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(drv~cyl)

ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy)) +
  facet_grid(.~cyl)

?facet_wrap

########################################## Geometric Objects ###
ggplot(data = mpg) +
  geom_point(mapping = aes(x=displ, y=hwy))

ggplot(data=mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy, group=drv))

ggplot(data=mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy, linetype = drv))

ggplot(data=mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy, color = drv), show.legend=FALSE)

ggplot(data=mpg, mapping = aes(x=displ, y=hwy)) +
  geom_point(mapping = aes(color=class)) +
  geom_smooth(data = filter(mpg, class == "subcompact"), mapping = aes(color=class), se=FALSE)

#note: se=FALSE removes the grey around the line, show.legend=FALSE removes legend

##################################### Statistical Transformations ###
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut))

#this is the same thing as the one above
ggplot(data=diamonds) +
  stat_count(mapping = aes(x=cut))

#display bar chart of proportion rather than count
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut, y=..prop.., group=1))

?stat_bin

#histograms
ggplot(diamonds, aes(carat)) +
  geom_histogram()

ggplot(diamonds, aes(carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(diamonds, aes(carat)) +
  geom_histogram(bins = 200)

# Map values to y to flip the orientation
ggplot(diamonds, aes(y = carat)) +
  geom_histogram()

# For histograms with tick marks between each bin
ggplot(diamonds, aes(carat)) +
  geom_bar() +
  scale_x_binned()

# Rather than stacking histograms, it's easier to compare frequency
ggplot(diamonds, aes(price, fill = cut)) +
  geom_histogram(binwidth = 500)

ggplot(diamonds, aes(price, colour = cut)) +
  geom_freqpoly(binwidth = 500)

# To make it easier to compare distributions with very different counts,
# put density on the y axis instead of the default count
ggplot(diamonds, aes(price, after_stat(density), colour = cut)) +
  geom_freqpoly(binwidth = 500)

####################################Position Adjustments ###
######FOR BAR CHARTS
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut, color=cut))

ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut, fill=cut))

ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut, fill=clarity))

#position=identity (causes overlap)
ggplot(data = diamonds, mapping = aes(x=cut, fill=clarity)) +
  geom_bar(alpha=1/5, position = "identity")

#position=fill (each stacked bar is same height, compare proportions)
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut, fill=clarity), position="fill")

#position=dodge (compare individual values)
ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut, fill=clarity), position="dodge")

######FOR SCATTERPLOTS
ggplot(data=mpg) +
  geom_jitter(mapping = aes(x=displ, y=hwy), width=0.5, height=0.5)

####################################### Coordinate Systems ###
#flip x and y
ggplot(data=mpg, mapping = aes(x=class, y=hwy)) +
  geom_boxplot() +
  coord_flip()

#Polar coordinates
bar <- ggplot(data=diamonds) +
  geom_bar(mapping = aes(x=cut, fill=cut), show.legend=FALSE, width=1) +
  theme(aspect.ratio=1) +
  labs(x=NULL, y=NULL)
bar
bar + coord_flip()
bar + coord_polar()

#titles
ggplot(data=mpg, mapping = aes(x=displ, y=hwy)) +
  geom_jitter() +
  labs(title = "title", subtitle="subtitle", caption="caption", tag="tag")