EPI_data <- read.csv('2010EPI_data.csv')
library(ggplot2)

# FIX ROW NAMES
names(EPI_data) <- as.character(unlist(EPI_data[1,])) # replace column names
EPI_data <- EPI_data[-1,] # get rid of names from rows
row.names(EPI_data) <- NULL # fix row ordering

# SET EPI_data AS DEFAULT 
attach(EPI_data) 	# sets the ‘default’ object
tf <- is.na(as.numeric(AIR_E))

# MODE FUNCTION
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# MEAN MEDIAN AND MODE
mmm <- function(x) {
  print(paste('Mean:',mean(x)))
  print(paste('Median:',median(x)))
  print(paste('Mode:',Mode(x)))
}

# AIR & WATER PROCESSING
AIR <- as.numeric(AIR_E[!tf])
WATER <- as.numeric(WATER_E[!tf])

mmm(AIR)
mmm(WATER)
boxplot(AIR,WATER,names=c("AIR_E","WATER_E"))


# NOX, SO2, ETC PROCESSING
NOX <- as.numeric(NOX_pt[!tf])
SO2 <- as.numeric(SO2_pt[!tf])
OZONE <- as.numeric(OZONE_pt[!tf])
WQI <- as.numeric(WQI_pt[!tf])

mmm(NOX)
mmm(SO2)
boxplot(OZONE,WQI,names=c("OZONE_pt","WQI_pt"))

# CLIMATE, AGRICULTURE, ETC PROCESSING
CLIM <- as.numeric(CLIMATE[!tf])
AGRI <- as.numeric(AGRICULTURE[!tf])
FISH <- as.numeric(FISHERIES[!tf])
NMVOC <- as.numeric(NMVOC_pt[!tf])

mmm(CLIM)
mmm(AGRI)
boxplot(FISH,NMVOC,names=c("FISHERIES","NMVOC_pt"))

# GENERATE SOME BOXPLOTS
ENV_H <- as.numeric(ENVHEALTH[!tf])
ECOS <- as.numeric(ECOSYSTEM[!tf])

boxplot(ENV_H,ECOS)
qqplot(ENVHEALTH,ECOSYSTEM)