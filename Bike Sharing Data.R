#reading files
day <- read.csv("C:/Technical Test/Technical Test/Question1 Data Set/day.csv")
hour <- read.csv("C:/Technical Test/Technical Test/Question1 Data Set/hour.csv")

summary(day) 
summary(hour)

str(hour)

hour1 <- hour
hour1$season <- as.numeric(hour$season)
hour1$dteday <- as.numeric(hour$dteday)
hour1$workingday <- as.numeric(hour$workingday)
hour1$weathersit <- as.numeric(hour$weathersit)
hour1$hr <- as.numeric(hour$hr)


reqd_vars <- subset(hour1, select=c(cnt,casual, registered,hr,temp,atemp,hum,windspeed,season,dteday,workingday,weathersit))
cor_dataset <- cor(reqd_vars)

library(corrplot)
corrplot(cor(reqd_vars),method='color',addCoef.col="black")

#analysing drivers for spring

spring <- subset(hour1, hour1$season==1)
reqd_vars1 <- subset(spring, select=c(cnt,casual, registered,hr,temp,atemp,hum,windspeed,dteday,workingday,weathersit))


require(caTools)
set.seed(101) 
sample = sample.split(spring$instant, SplitRatio = .75)
train = subset(spring, sample == TRUE)
test  = subset(spring, sample == FALSE)


#fitting regression to identify drivers

lmBikeRent <- lm(cnt~dteday+hr+holiday+temp+hum+windspeed, data = train)
summary(lmBikeRent)

corrplot(cor(reqd_vars1),method='color',addCoef.col="black")


         