install.packages("ISLR")
library(ISLR)

head(Hitters)
dim(Hitters)

is.na(Hitters)

HittersData <- na.omit(Hitters)

dim(HittersData)
head(HittersData)
SalaryPredictModel <- lm(Salary ~., data = HittersData)
summary(SalaryPredictModel)

cooksD <- cooks.distance(SalaryPredictModel)
influential <- cooksD[(cooksD > (3 * mean(cooksD, na.rm = TRUE)))]

names_of_influential <- names(influential)
names_of_influential
outliers <- HittersData[names_of_influential,]

library(dplyr)

Hitters_Without_Outliers <- HittersData %>% anti_join(outliers)

SalaryPredictModel2 <- lm(Salary ~., data = Hitters_Without_Outliers)
summary(SalaryPredictModel2)
