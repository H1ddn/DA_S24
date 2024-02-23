EPI_data <- read.csv('2010EPI_data.csv')
library(ggplot2)

# FIX ROW NAMES
names(EPI_data) <- as.character(unlist(EPI_data[1,])) # replace column names
EPI_data <- EPI_data[-1,] # get rid of names from rows
row.names(EPI_data) <- NULL # fix row ordering

# SET EPI_data AS DEFAULT 
attach(EPI_data) 	# sets the ‘default’ object
tf <- is.na(as.numeric(AIR_E))

# PREPROCESS
ENVHEALTH <- as.numeric(ENVHEALTH[!tf])
DALY <- as.numeric(DALY[!tf])
AIR_H <- as.numeric(AIR_H[!tf])
WATER_H <- as.numeric(WATER_H[!tf])
AIR_E <- as.numeric(AIR_E[!tf])
CLIMATE <- as.numeric(CLIMATE[!tf])
ECOSYSTEM <- as.numeric(ECOSYSTEM[!tf])

# REGRESSION
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H,names=c('ENV','DALY','AIR','WATER'))
lmENVH <- lm(ENVHEALTH~DALY+AIR_H+WATER_H)

lmENVH
summary(lmENVH)
cENVH <- coef(lmENVH)

# PREDICTS
DALYNEW <- c(seq(5,95,5))
AIR_HNEW <- c(seq(5,95,5))
WATER_HNEW <- c(seq(5,95,5))
NEW <- data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)

pENV <- predict(lmENVH,NEW,interval="prediction")
cENV <- predict(lmENVH,NEW,interval="confidence")

# REGRESSION FOR AIR
Model1 <- lm(CLIMATE~DALY+ENVHEALTH+WATER_H)

Model1
summary(Model1)
cAIRE <- coef(Model1)

# REGRESSION FOR CILMATE
Model2 <- lm(CLIMATE~DALY+ENVHEALTH+WATER_H)

Model2
summary(Model2)
cCLIM <- coef(Model2)

# SHAPIRO-WILK
shapiro.test(ENVHEALTH)
shapiro.test(ECOSYSTEM)

shapiro.test(ENVHEALTH)
shapiro.test(DALY)
shapiro.test(AIR_H)
shapiro.test(WATER_H)
# SAMPLE SIZE IS 163, BETWEEN 5 and 5000
# p-val IS <0.5 for EVERY TEST
# W IS HIGH (~0.9) FOR EVERY TEST
# THEY ARE ALL CLOSE TO NORMAL DISTRIBUTION!