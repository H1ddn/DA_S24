RegData <- read.csv('dataset_multipleRegression.csv')

# No preprocessing needed, everythin looks great :)
attach(RegData)

lmROLL <- lm(ROLL~UNEM+HGRAD)

summary(lmROLL)

# Predict ROLL if Unem = 7% and HGrad = 90,000
Punemp <- 7
Phgrad <- 90000
newdat <- data.frame(Punemp,Phgrad)
colnames(newdat) <- c('UNEM','HGRAD')
predict(lmROLL,newdat)

# OK NOW ADD PER CAPITA
lmROLLCPTA <- lm(ROLL~UNEM+HGRAD+INC)

summary(lmROLLCPTA)

# Predict ROLL if Unem = 7%, HGrad = 90k, and INC = 25k
Pinc <- 25000
newdat2 <- data.frame(Punemp,Phgrad,Pinc)
colnames(newdat2) <- c('UNEM','HGRAD','INC')
predict(lmROLLCPTA,newdat2)

