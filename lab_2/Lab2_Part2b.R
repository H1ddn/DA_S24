library(ggplot2)
library(nycflights13)

View(flights)

# plot delay by carrier
ggplot(flights, aes(x=carrier, y=dep_delay)) + stat_summary(FUN=mean, geom="col") + ylab("average departure delay")

# delay difference by distance
ggplot(flights, aes(x=arr_delay, y=dep_delay, color=distance)) + geom_point()

# delay difference by month
ggplot(flights, aes(x=month, y=dep_delay, group=1)) + stat_summary(FUN=mean, geom="path")

# flight difference by month
ggplot(flights, aes(x=month, fill=month )) + 
  geom_bar( ) +
  scale_color_gradient(low = "#132B43", high = "#56B1F7") +
  theme(legend.position="none")

# average delay by carrier
ggplot(flights, aes(x=carrier, y=arr_delay)) + stat_summary(FUN=mean, geom="col")

# tot flights for each airport
ggplot(flights, aes(x=origin, fill=origin )) + 
  geom_bar( ) +
  scale_color_gradient(low = "#132B43", high = "#56B1F7") +
  theme(legend.position="none")

# Distance vs Air Time
ggplot(flights, aes(x=distance, y=air_time)) + geom_point()

lmDist <- lm(flights$distance~flights$air_time)
summary(lmDist)

# avg airtime vs airline
ggplot(flights, aes(x=carrier, y=air_time)) + geom_boxplot()

