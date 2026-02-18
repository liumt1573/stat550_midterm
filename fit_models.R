# 550 midterm project - model fitting
library(nlme)
library(MASS)

dat1 <- read.table('/Users/nickson/Downloads/All data - Original.csv',
                   header=TRUE,sep = ",")
dat1$Mine <- factor(dat1$Mine)
dat1$Site <- factor(dat1$Site)
dat1$Storage <- factor(dat1$Storage)
dat2 <- groupedData(OD ~ Time|Primary_Key, data = dat1)

# Model 1 - polynomial LME model
m1.fit1<- lme(OD ~ Time + I(Time^2) + Mine*Time + Storage*Time + Cu*Time
              + Mine*I(Time^2) + Storage*I(Time^2) + Cu*I(Time^2),
              random = ~Time + I(Time^2)|Primary_Key, data = dat1)
# collinearity b/w Mine and Site so can't use both



# model 2 - linear model fitting variables
wide1 <- read.table("/Users/nickson/Downloads/wider.csv",
                    header=TRUE, sep = ",", fill = TRUE)
wide1$Mine <- factor(wide1$Mine)
wide1$Site <- factor(wide1$Site)
wide1$Storage <- factor(wide1$Storage)
wide2 <- wide1[wide1$mu_max > 0,]

m2.fit_lag <- lm(lag_time ~ Mine + Storage + Cu, data = wide1)
m2.fit_max <- lm(Maximum_Concentration ~ Mine + Storage + Cu, data = wide1)
m2.fit_grow <- lm(mu_max ~ Mine + Storage + Cu, data = wide2)

# check diagnostic plots: lag time
plot(m2.fit_lag,which=1)
m2.fit_lag <- lm((lag_time)^0.3 ~ Mine + Storage + Cu, data = wide1)
plot(m2.fit_lag,which=2)
# still looks awful...

# check diagnostic plots: max conc
plot(m2.fit_max,which=1)
plot(m2.fit_max,which=2)
m2.fit_max <- lm(Maximum_Concentration^0.1 ~ Mine + Storage + Cu, 
                 data = wide1[wide1$Maximum_Concentration > 0,])
boxcox(m2.fit_max, lambda = seq(0, 0.2, len = 21),)

# check diagnostic plots: growth rate
boxcox(m2.fit_grow,  lambda = seq(0, 0.2, len = 21),)
# Box-Cox transformation suggests scaling response by power of 0.125?
m2.fit_grow <- lm(mu_max^0.125 ~ Mine + Storage + Cu, data = wide2)
# NOTE: this is without 0's. Boxcox test needs response strictly positive.
# this transformation looks much worse if 0s are included
plot(m2.fit_grow,which=1)
plot(m2.fit_grow,which=2)

# model 3 - NLME growth curve model