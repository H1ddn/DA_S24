library(ggplot2)
attach(diamonds)

View(diamonds)
sum(is.na(price))

# histogram 
hist(price, breaks=10)
hist(price, breaks=250)

# bar plot color & 
ggplot(diamonds, aes(x=as.factor(color), fill=as.factor(color) )) + 
  geom_bar( ) +
  scale_color_gradient(low = "#132B43", high = "#56B1F7") +
  theme(legend.position="none")

ggplot(diamonds, aes(x=as.factor(clarity), fill=as.factor(clarity) )) + 
  geom_bar( ) +
  scale_color_gradient(low = "#132B43", high = "#56B1F7") +
  theme(legend.position="none")

# scatterplot
ggplot(diamonds, aes(x=price, y=carat, color=clarity)) + geom_point()

# cut vs price
aggregate(x=price, by=list(cut), FUN=mean)
# highest price average by cut is Premium

# color by price and carat
aggregate(x=price, by=list(color), FUN=mean)
aggregate(x=price, by=list(color), FUN=median)
aggregate(x=carat, by=list(color), FUN=mean)
aggregate(x=carat, by=list(color), FUN=median)

# boxplot diamonds, cut
ggplot(diamonds, aes(x=cut, y=price)) + geom_boxplot()
ggplot(diamonds, aes(x=cut, y=price, color=cut)) + geom_boxplot()

# average price by clarity & color
ggplot(diamonds, aes(x=clarity, y=price)) + stat_summary(FUN=mean, geom="col") + ylab("average price")
ggplot(diamonds, aes(x=color, y=price)) + stat_summary(FUN=mean, geom="col") + ylab("average price")
