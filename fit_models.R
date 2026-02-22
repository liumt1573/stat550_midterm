# 550 midterm project - model fitting
library(nlme)
library(MASS)
library(here)

dat1 <- read.table(here("original.csv"),
                   header=TRUE,sep = ",")
dat1$Mine <- factor(dat1$Mine)
dat1$Site <- factor(dat1$Site)
dat1$Storage <- factor(dat1$Storage)

# get IDs with "no growth" - code from Arqam
library(dplyr)
  
  curve_stats <- dat1 %>%
  group_by(Primary_Key) %>%
  summarise(
    max_OD = max(OD, na.rm = TRUE),
    min_OD = min(OD, na.rm = TRUE),
    range_OD = max_OD - min_OD,
    baseline_OD = mean(OD[Time == min(Time)], na.rm = TRUE),
    fold_change = max_OD / mean(OD[Time == min(Time)], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(grew_data = (range_OD > 0.1) & (fold_change > 1.5))

cat("Heuristic growth:", sum(curve_stats$grew_data), "\n")
cat("Heuristic no-growth:", sum(!curve_stats$grew_data), "\n")

growth_keys <- curve_stats$Primary_Key[curve_stats$grew_data == TRUE]
# get list of IDs that have growth
growth_ids <- rep(NA,length(dat1$ID))
for(i in 1:length(growth_ids)) {
 if(dat1$Primary_Key[i] %in% growth_keys) growth_ids[i] <- dat1$ID[i]
}
# growth_ids <- rep(0,length(unique(dat1$ID)))
# for(i in 1:length(growth_ids)){
#   growth_ids[i] <- dat1$ID[dat1$Primary_Key==growth_keys[i]][1]
# }
u_growth_ids <- unique(growth_ids[!is.na(growth_ids)])

is_growth <- dat1$Primary_Key %in% growth_keys
dat2 <- groupedData(OD ~ Time|Primary_Key, data = dat1[is_growth,])

# Model 1 - polynomial LME model
m1.fit1<- lme(OD ~ Time + I(Time^2) + Mine + Storage + Cu 
              + Mine*Time + Storage*Time + Cu*Time
              + Mine*I(Time^2) + Storage*I(Time^2) + Cu*I(Time^2),
              random = ~Time + I(Time^2)|Primary_Key, data = dat2)
# Mine is not significant, do model selection
# fit 2: no mine
m1.fit2 <- lme(OD ~ Time + I(Time^2) + Storage*Time + Cu*Time
              + Storage*I(Time^2) + Cu*I(Time^2),
              random = ~Time + I(Time^2)|Primary_Key, data = dat2)
# fit 3: no storage - AIC 1826.381
m1.fit3 <- lme(OD ~ Time + I(Time^2) + Mine*Time + Cu*Time
               + Mine*I(Time^2) + Cu*I(Time^2),
               random = ~Time + I(Time^2)|Primary_Key, data = dat2)
# fit 4: no Cu - AIC 1840.376
m1.fit4 <- lme(OD ~ Time + I(Time^2) + Mine*Time + Storage*Time
               + Mine*I(Time^2) + Storage*I(Time^2),
               random = ~Time + I(Time^2)|Primary_Key, data = dat2)
# fit 5: no mine and no storage
m1.fit5 <- lme(OD ~ Time + I(Time^2)  + Cu*Time + Cu*I(Time^2),
               random = ~Time + I(Time^2)|Primary_Key, data = dat2)

# plot fitted vs actual
set.seed(222) # randomly sample 8 
toplot <- sample(unique(growth_keys),8)
par(mfrow = c(2,4))
for(i in 1:length(toplot)) {
  idx <- dat2$Primary_Key==toplot[i]
  t <- dat2$Time[idx]
  od <- dat2$OD[idx]
  fit <- fitted(m1.fit2)[idx]
  ii <- order(t,od,fit) # sort data by time before plotting
  plotdata <- rbind(t,od,fit)[,ii]
  # plot actual values
  plot(plotdata[1,],plotdata[2,], xlab = 'Time',ylab = "OD",main = toplot[i])
  lines(plotdata[1,],plotdata[3,]) # plot fitted values
}

# plot averages
plot_cu <- rep(unique(dat2$Cu),each=2)
dim(plot_cu) <- c(2,6)
plot_storage <- rep(c(0,1),6)
dim(plot_storage)<- c(2,6)

par(mfrow = c(2,6))
coef <- fixef(m1.fit2)
t <- sort(unique(dat2$Time))
avgcurve <- function(st,cu,t) {
  return(coef[1] + coef[2]*t + coef[3]*t^2 + coef[4]*st + coef[5]*cu
         + coef[6]*t*st+ coef[7]*t*cu +coef[8]*st*(t^2) + coef[9]*cu*(t^2))
}
for(i in 1:2){
  for(j in 1:6){
    plot(t,avgcurve(plot_storage[i,j],plot_cu[i,j],t),type = 'l'
         ,ylab = 'OD', ylim = c(0,1.5), 
         main = paste('St',plot_storage[i,j],'Cu',plot_cu[i,j]))
  }
}

# model 2 - linear model fitting variables
wide1 <- read.table("/Users/nickson/Downloads/wider.csv",
                    header=TRUE, sep = ",", fill = TRUE)
wide1$Mine <- factor(wide1$Mine)
wide1$Site <- factor(wide1$Site)
wide1$Storage <- factor(wide1$Storage)
# exclude zero-growth samples
wide2 <- wide1[wide1$ID %in% u_growth_ids,]

# max conc
m2.fit_max <- lm(Maximum_Concentration^0.3 ~ Mine + Storage + Cu, 
                 data = wide2)
par(mfrow=c(1,2))
plot(m2.fit_max,which=1)
plot(m2.fit_max,which=2)
boxcox(m2.fit_max, lambda = seq(0, 0.5, len = 21),)

# growth rate
m2.fit_grow <- lm(mu_max^0.25 ~ Mine + Storage + Cu, data = wide2)
plot(m2.fit_grow,which=1)
plot(m2.fit_grow,which=2)
boxcox(m2.fit_grow,  lambda = seq(0, 0.5, len = 21),)

# lag time
m2.fit_lag <- lm(lag_time ~ Mine + Storage + Cu, data = wide2)
par(mfrow=c(1,2))
plot(m2.fit_lag,which=1)
plot(m2.fit_lag,which=2)
boxcox(m2.fit_lag, lambda = seq(0, 0.2, len = 21),)
