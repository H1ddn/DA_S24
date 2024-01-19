EPI_data <- read.csv('2010EPI_data.csv')
#or
#EPI_data <- read.xlsx(”<path>/2010EPI_data.xlsx")
# Note: replace default data frame name – cannot start with numbers!
#
# FIX ROW NAMES
names(EPI_data) <- as.character(unlist(EPI_data[1,])) # replace column names
EPI_data <- EPI_data[-1,] # get rid of names from rows
row.names(EPI_data) <- NULL # fix row ordering

View(EPI_data)

attach(EPI_data) 	# sets the ‘default’ object
fix(EPI_data) 	# launches a simple data editor

EPI 			# prints out values EPI_data$EPI
tf <- is.na(EPI) # records True values if the value is NA
E <- as.numeric(EPI[!tf]) # filters out NA values, new array

# EXERCISE 1
# EPI
summary(E)
fivenum(E,na.rm=TRUE)
stem(E)
hist(E)

# HISTOGRAM
hist(E, seq(30., 95., 1.0), prob=TRUE)
lines(density(E,na.rm=TRUE,bw=1.))
rug(E)

# Cumulative Density Function
plot(ecdf(E), do.points=FALSE, verticals=TRUE)

# Quantile-Quantile
par(pty="s")
qqnorm(E); qqline(E)


# QQ against generalizing dist
qqplot(qt(ppoints(250), df = 5), E, xlab = "Q-Q plot for t
dsn")
qqline(E)

# WATER_H
W_H <- as.numeric(WATER_H[!tf]) # filters out NA values, new array
summary(W_H)
fivenum(W_H,na.rm=TRUE)
stem(W_H)
hist(W_H)

# HISTOGRAM
hist(W_H, seq(0., 100., 1.0), prob=TRUE)
lines(density(W_H,na.rm=TRUE,bw=1.))
rug(W_H)

# Cumulative Density Function
plot(ecdf(W_H), do.points=FALSE, verticals=TRUE)

# BIODIVERSITY
BD <- as.numeric(BIODIVERSITY[!tf])
summary(BD)
fivenum(BD,na.rm=TRUE)
stem(BD)
hist(BD)

# HISTOGRAM
hist(BD, seq(0., 100., 1.0), prob=TRUE)
lines(density(BD,na.rm=TRUE,bw=1.))
rug(BD)

# Quantile-Quantile
par(pty="s")
qqnorm(BD); qqline(BD)

# COMPARE
boxplot(E,BD)
qqplot(E,BD)
qqplot(W_H,BD)
qqplot(E,W_H)



# EXERCISE 2



#other data
GRUMP_data <- read.csv('GPW3_GRUMP_SummaryInformation_2010.csv')


