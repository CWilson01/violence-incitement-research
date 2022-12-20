## Code used to produce data analysis for Inciting Violence: The Impact of Internet Propaganda on Terrorist Outcomes
## Submission for the Dr. Jonathan Fine Essay Competition focusing on the topic of "Tech and Terrorism"
## Full paper can be found here: https://dx.doi.org/10.2139/ssrn.4035908 
## Author: Cody Wilson (https://wonksecurity.com)
## © CC BY NC 3.0 ©
## You are free to adapt, copy or redistribute the material, providing you attribute appropriately and do not use the material for commercial purposes.


## Individual data sources located in the original Excel spreadsheet.
## ACLED data courtesy of https://acleddata.com/
## START data courtesy of https://start.umd.edu/gtd/






## Data Analysis ##########################################################

# Importing and converting to time series
library(readxl)
Tech_and_Terrorism_Dataset <- read_excel("Wilson_Inciting Violence Dataset_GH.xlsx", 
                                         sheet = "Values for R Import")

library(tseries)
library(tfplot)
library(lmtest)
library(forecast)
library(pspearman)
library(car)
# Scale transformations
scaled_attack <- as.data.frame(scale(Tech_and_Terrorism_Dataset$total_real_world))
scaled_member <- as.data.frame(scale(Tech_and_Terrorism_Dataset$member_estimate))
scaled_online <- as.data.frame(scale(Tech_and_Terrorism_Dataset$online_estimate))
scaled_weighted <- as.data.frame(scale(Tech_and_Terrorism_Dataset$weighted_online))



# converting to time series
onlinetime <- ts(Tech_and_Terrorism_Dataset$online_estimate, 
                 start = min(Tech_and_Terrorism_Dataset$year), 
                 end = max(Tech_and_Terrorism_Dataset$year), 
                 frequency = 1)

membertime <- ts(Tech_and_Terrorism_Dataset$member_estimate, 
                 start = min(Tech_and_Terrorism_Dataset$year), 
                 end = max(Tech_and_Terrorism_Dataset$year), 
                 frequency = 1)

attacktime <- ts(Tech_and_Terrorism_Dataset$total_real_world, 
                 start = min(Tech_and_Terrorism_Dataset$year), 
                 end = max(Tech_and_Terrorism_Dataset$year), 
                 frequency = 1)

scaled_attacktime <- ts(scaled_attack, 
                        start = min(Tech_and_Terrorism_Dataset$year), 
                        end = max(Tech_and_Terrorism_Dataset$year), 
                        frequency = 1)

scaled_membertime <- ts(scaled_member, 
                        start = min(Tech_and_Terrorism_Dataset$year), 
                        end = max(Tech_and_Terrorism_Dataset$year), 
                        frequency = 1)

scaled_onlinetime <- ts(scaled_online, 
                        start = min(Tech_and_Terrorism_Dataset$year), 
                        end = max(Tech_and_Terrorism_Dataset$year), 
                        frequency = 1)

scaled_weightedtime <- ts(scaled_weighted, 
                          start = min(Tech_and_Terrorism_Dataset$year), 
                          end = max(Tech_and_Terrorism_Dataset$year), 
                          frequency = 1)



# Exploring the data
plot(onlinetime)
abline(reg=lm(onlinetime~time(onlinetime)))
summary(onlinetime)
plot(membertime)
abline(reg=lm(membertime~time(membertime)))
summary(membertime)
plot(attacktime)
abline(reg=lm(attacktime~time(attacktime)))
summary(attacktime)

ts.plot(onlinetime, membertime, scaled_attacktime, col=c("blue", "red", "green"))
ts.plot(scaled_onlinetime, scaled_membertime, scaled_attacktime, scaled_weightedtime, 
        col=c("blue", "red", "green", "orange"),
        ylab= "Scaled Value", 
        main = "Scaled Membership, Attacks, Weighted and Unweighted Online Activity, 1998-2018") #-------SHOW ME


# Time Series Tests for Each Variable
acf(scaled_onlinetime)
acf(scaled_membertime)
acf(scaled_attacktime)

pacf(onlinetime)
pacf(membertime)
pacf(attacktime)

adf.test(scaled_onlinetime) # higher than 0.5 p-value indicated non-stationarity
adf.test(scaled_membertime) # higher than 0.5 p-value indicated non-stationarity
adf.test(scaled_attacktime) # higher than 0.5 p-value indicated non-stationarity
kpss.test(scaled_onlinetime) # lower than 0.5 p-value indicated non-stationarity
kpss.test(scaled_membertime) # lower than 0.5 p-value indicated non-stationarity
kpss.test(scaled_attacktime) # lower than 0.5 p-value indicated non-stationarity

#-------------------------------------------------------------------------------
# Detrending of scale transformations: Method 2 (residuals) 
trendonline <- lm(scaled_onlinetime~time(scaled_onlinetime))
detrendonline <- residuals(trendonline)
plot.ts(detrendonline)                  
summary(detrendonline)

trendmember <- lm(scaled_membertime~time(scaled_membertime))         
detrendmember <- residuals(trendmember)              
plot.ts(detrendmember)   
summary(detrendmember)

trendattack <- lm(scaled_attacktime~time(scaled_attacktime))              
detrendattack <- residuals(trendattack)              
plot.ts(detrendattack) 
summary(detrendattack)

trendweighted <- lm(scaled_weightedtime~time(scaled_weightedtime))
detrendweighted <- residuals(trendweighted)
plot.ts(detrendweighted)                  
summary(detrendweighted)

ts.plot(ts(detrendmember), ts(detrendonline), ts(detrendattack), ts(detrendweighted), 
        col=c("blue", "red", "green", "orange"))

# Measurements of Detrended Correlation ########################################

#acf(ts.intersect(ts(detrendonline), ts(detrendmember)))
#acf(ts.intersect(ts(detrendonline), ts(detrendattack)))
acf(ts.intersect(ts(detrendmember), ts(detrendattack)))
acf(ts.intersect(ts(detrendweighted), ts(detrendmember)))
acf(ts.intersect(ts(detrendweighted), ts(detrendattack)))

ccf(detrendweighted, detrendmember)
ccf(detrendweighted, detrendattack)
ccf(detrendmember, detrendattack)

spearman.test(detrendweighted, detrendmember) #rho of 0.589 and p-value 0.005, significant

spearman.test(detrendweighted, detrendattack) #rho of 0.84 and tiny p-value
spearman.test(detrendonline, detrendattack)

spearman.test(detrendmember, detrendattack) #rho of 0.84 and tiny p-value, interestingly same value as online and attack

# Method 2 De-trended Scaled Linear Models (residual models) 
scale_onlineattack <- lm(detrendattack~detrendonline)
summary(scale_onlineattack) # 68% of variation in attack explained by online, tiny p-value

scale_weightedattack <- lm(detrendattack~detrendweighted) #######################
summary(scale_weightedattack)

scale_weightedmember <- lm(detrendmember~detrendweighted)
summary(scale_weightedmember)

scale_onlinemember <- lm(detrendmember~detrendonline)
summary(scale_onlinemember) # 17% of variation in membership explained by online, .03 p-value

scale_memberattack <- lm(detrendattack~detrendmember)
summary(scale_memberattack) # 66% of variation in attack explained by membership, tiny p-value

# Trying a polynomial regression model on detrended time series
detrendonlinesqr <- detrendonline ^ 2
detrendonlinecub <- detrendonline ^ 3
detrendonlinequad <- detrendonline ^ 4
detrendmembersqr <- detrendmember ^ 2
detrendweightedsqr <- detrendweighted ^ 2
detrendattacksqr <- detrendattack ^ 2


weight_attackreg <- lm(detrendattack ~ detrendweighted + detrendmember + detrendmembersqr)
summary(weight_attackreg)

scale_attackreg <- lm(detrendattack ~ detrendonline + detrendmember)
summary(scale_attackreg) #93% of variation in attack explained by combination of online and membership, tiny p-value for both

scale_memberreg <- lm(detrendmember ~ detrendweighted + detrendattack + detrendweightedsqr + detrendattacksqr)
summary(scale_memberreg) # 82.6% of variation in membership explained by combination of online and attack, tiny p-value for both 

weight_onlinereg <- lm(detrendonline ~ detrendattack + detrendmember)
summary(weight_onlinereg)

# Model Tests for Method 2
plot(fitted(scale_onlineattack), resid(scale_onlineattack)) #evenly distributed but clear pattern, indicating need for better model
plot(fitted(scale_onlinemember), resid(scale_onlinemember))#evenly distributed and less clear pattern, indicating need for better model

plot(fitted(scale_attackreg), resid(scale_attackreg))# evenly distributed without clear pattern, solid model

qqnorm(resid(scale_onlineattack))
qqline(resid(scale_onlineattack)) # Minimal deviations, suggesting normal distribution

qqnorm(resid(scale_attackreg)) #Slight deviation at the tails
qqline(resid(scale_attackreg))

# Tests for Online and Attack Model
residualPlots(scale_onlineattack)
hist(scale_onlineattack$residuals)
qqPlot(scale_onlineattack$residuals)
shapiro.test(scale_onlineattack$residuals) #confirms normality 
spreadLevelPlot(scale_onlineattack)
ncvTest(scale_onlineattack) # confirms constant variance

# Tests for Online and Membership Model 
residualPlots(scale_onlinemember) # suggests need for a polynomial model
hist(scale_onlinemember$residuals)
qqPlot(scale_onlinemember$residuals)
shapiro.test(scale_onlinemember$residuals) #confirms normality 
spreadLevelPlot(scale_onlinemember)
ncvTest(scale_onlinemember) # confirms constant variance

# Tests for Member and Attack Model
residualPlots(scale_memberattack) 
hist(scale_memberattack$residuals)
qqPlot(scale_memberattack$residuals)
shapiro.test(scale_memberattack$residuals) #not normally distributed
spreadLevelPlot(scale_memberattack)
ncvTest(scale_memberattack) # very close to not having constant variance

residualPlots(weight_onlinereg) 
hist(weight_onlinereg$residuals)
qqPlot(weight_onlinereg$residuals)
shapiro.test(weight_onlinereg$residuals) #not normally distributed
spreadLevelPlot(weight_onlinereg)
ncvTest(weight_onlinereg)


# Test for multiple Attack, Member, Online Model
residualPlots(scale_attackreg) 
hist(scale_attackreg$residuals)
qqPlot(scale_attackreg$residuals)
shapiro.test(scale_attackreg$residuals) #confirms normality
spreadLevelPlot(scale_attackreg)
ncvTest(scale_attackreg) # confirms constant variance

residualPlots(weight_attackreg) 
hist(weight_attackreg$residuals)
qqPlot(weight_attackreg$residuals)
shapiro.test(weight_attackreg$residuals) #confirms normality
spreadLevelPlot(weight_attackreg)
ncvTest(weight_attackreg) # confirms constant variance


# Test for multiple Member, Attack, Online Model
residualPlots(scale_memberreg) # suggests need for polynomial on online variable
hist(scale_memberreg$residuals)
qqPlot(scale_memberreg$residuals)
shapiro.test(scale_memberreg$residuals) #confirms normality
spreadLevelPlot(scale_memberreg)
ncvTest(scale_memberreg) # lacks constant variance


poly_onlinemember <- lm(detrendmember~detrendonline + detrendonlinecub)
summary(poly_onlinemember) #increased r-squar, both values significant, similar residuals, useful-------------------------------------


poly_memberreg <- lm(detrendmember ~ detrendonline + detrendattack + detrendonlinesqr)
summary(poly_memberreg) # increased r-square, all values significant, similar residuals, useful ----------------------

# Tests for polynomial online and member model - PASSED, Use in Write Up
residualPlots(poly_onlinemember) # non-linearity remains, highly significant
hist(poly_onlinemember$residuals)
qqPlot(poly_onlinemember$residuals)
shapiro.test(poly_onlinemember$residuals) #confirms normality
spreadLevelPlot(poly_onlinemember)
ncvTest(poly_onlinemember) # confirms constant variance

# Tests for polynomial member, online, attack model - PASSED
residualPlots(poly_memberreg) # non-linearity now removed
hist(poly_memberreg$residuals)
qqPlot(poly_memberreg$residuals)
shapiro.test(poly_memberreg$residuals) #confirms normality
spreadLevelPlot(poly_memberreg)
ncvTest(poly_memberreg) # confirms constant variance




#--------------------------STOP HERE, Discarded Models--------------------------

#determining number of differences to stationarity
#ndiffs(scaled_attacktime, test = c("kpss", "adf", "pp")) # 1 necessary
#ndiffs(scaled_membertime, test = c("kpss", "adf", "pp")) # 1 necessary
#ndiffs(scaled_onlinetime, test = c("kpss", "adf", "pp")) # 1 necessary

# Employ first differencing function on each dataset METHOD 1 
#difsca_onlinetime <- diff(scaled_onlinetime)
#summary(difsca_onlinetime)
#difsca_membertime <- diff(scaled_membertime)
#summary(difsca_membertime)
#difsca_attacktime <- diff(scaled_attacktime)
#summary(difsca_attacktime)
#ts.plot(difsca_onlinetime, difsca_membertime, difsca_attacktime, 
  #col=c("blue", "red", "green"))

# Autocorrelation function on differenced time series
#acf(difsca_onlinetime) # Values after zero within acceptable range
#acf(difsca_membertime) # Values after zero within acceptable range
#acf(difsca_attacktime) # Values after zero within acceptable range

#adf.test(difsca_onlinetime) # higher than 0.5 p-value indicated non-stationarity
#adf.test(difsca_membertime) # higher than 0.5 p-value indicated non-stationarity
#adf.test(difsca_attacktime) # higher than 0.5 p-value indicated non-stationarity
#kpss.test(difsca_onlinetime) # lower than 0.5 p-value indicated non-stationarity
#kpss.test(difsca_membertime) # lower than 0.5 p-value indicated non-stationarity
#kpss.test(difsca_attacktime)

# Data remain non-stationary indicating best options for analysis are 
#non-parametric correlation and regression on first differences

# Measurements of Method 1 Differenced Correlation----------------------------

#acf(ts.intersect(ts(difsca_onlinetime), ts(difsca_membertime)))
#acf(ts.intersect(ts(difsca_onlinetime), ts(difsca_attacktime)))
#acf(ts.intersect(ts(difsca_membertime), ts(difsca_attacktime)))

#ccf(difsca_onlinetime, difsca_membertime)
#ccf(difsca_onlinetime, difsca_attacktime)

#Non-parametric correlation of Method 1 Differenced Time Series
#spearman.test(difsca_onlinetime, difsca_membertime) #rho of 0.148 and p-value 0.52, not significant
#spearman.test(difsca_onlinetime, difsca_attacktime) #rho of 0.34 and p-value 0.14, not significant
#spearman.test(difsca_membertime, difsca_attacktime) #rho of 0.356 and p-value of 0.12, not significant

# Linear Models of Method 1 Differenced Time Series
#diff_onlineattack <- lm(difsca_attacktime~difsca_onlinetime)
#summary(diff_onlineattack) # 16% of variation in attacks explained by online, .04 p-value

#diff_onlinemember <- lm(difsca_membertime~difsca_onlinetime)
#summary(diff_onlinemember) # No relationship, .1 p-value

#diff_memberattack <- lm(difsca_attacktime~difsca_membertime)
#summary(diff_memberattack) # 17% of variation in attack explained by membership, .03 p-value

#diff_attackreg <- lm(difsca_attacktime ~ difsca_onlinetime + difsca_membertime)
#summary(diff_attackreg) # 61% of variation in attacks explained by combination of online and membership, tiny p-value of 0.0001 for both

#diff_memberreg <- lm(difsca_membertime ~ difsca_onlinetime + difsca_attacktime)
#summary(diff_memberreg) # 57% of variation in membership explained by combination of online and attacks, tiny p-value of 0.0002 for both

# Testing Linear Models of Method 1 Differenced Time Series
#plot(fitted(diff_onlineattack), resid(diff_onlineattack)) #not evenly distributed and clear pattern, indicating need for better model
#plot(fitted(diff_onlinemember), resid(diff_onlinemember))#not evenly distributed and clear pattern, indicating need for better model

#plot(fitted(diff_attackreg), resid(diff_attackreg))#not evenly distributed and clear pattern, indicating need for better model 

#qqnorm(resid(diff_onlineattack))
#qqline(resid(diff_onlineattack)) # Deviation at the start and end showing non normal distribution

#qqnorm(resid(diff_attackreg)) #Slight deviation at the tails
#qqline(resid(diff_attackreg))

# Non-linear Models of Method 1 Differenced Time Series
#non_onlineattack <- nls(difsca_attacktime ~ c * difsca_onlinetime ^ z, start = c(c=0.1, z = 0.27))






#-------------Pre-analysis calculations and scratch pad-------------------------



## Simple linear sequence used for multi-year estimates of activity/membership.
## x = value given while y = timeframe plus one.
## seq(from = 0, to = x, length.out = y) - original used
#seq(from = 1000, to = 18000, length.out = 4)

#x <- 53781/4
#y <- 40220/2
#z <- 34.13 * 2 / 100
#a <- 13.59 * 2 / 100
#break1 <- 2.14 * 2 / 100

#big <- 0.6826 * 20110
#medium <- 0.2718 * 20110
#small <- 0.0428 * 20110

#bigdiv <- big / 2
#mediumdiv <- medium / 2
#smalldiv <- small / 2

#sum(big, medium, small, y)

#Prepping additional ACLED data to look through
#library(readr)
#acled_data <- read_csv("2019-01-01-2019-12-31.csv")
#View(acled_data)

#library(dplyr)
#df <- acled_data

#library(writexl)
#write_xlsx(acled_data, "acled_data_2019_full_original_uncleaned.xlsx")

# Random pre-analysis calculations
# Yearly membership totals
#ninetyeight <- 750 + 400 + 200 + 8400 + 2000 + 7000
#ninetynine <- 875 + 415 + 200 + 8700 + 2000 + 7000
#y2k <- 1250 + 500 + 250 + 300 + 9000 + 2000 + 7000
#y2k1 <- 1000 + 800 + 500 + 350 + 1000 + 2500 + 48 + 9300 + 9000 + 2000
#y2k2 <- 450 + 5000 + 500 + 500 + 2500 + 75 + 9600 + 2000 + 8000
#y2k3 <- 350 + 12000 + 300 + 750 + 2500 + 9900 + 7000 + 50 + 2000 + 1000
#y2k4 <- 300 + 18000 + 500 + 750 + 3000 + 2500 + 400 + 300 + 10200 + 8500 + 3000
#y2k5 <- 300 + 1000 + 700 + 1000 + 3000 + 2000 + 2500 + 300 + 10500 + 12000 + 5000
#y2k6 <- 350 + 1000 + 700 + 400 + 750 + 3000 + 6000 + 2500 + 300 + 10500 + 11000 + 17000
#y2k7 <- 300 + 850 + 500 + 900 + 600 + 1000 + 3000 + 7500 + 10400 + 2500 + 1500 + 300 + 11769 + 19000 + 5000
#y2k8 <- 300 + 750 + 200 + 700 + 3000 + 500 + 1175 + 3000 + 3000 + 7500 + 2240 + 600 + 300 + 11000 + 22000 + 7500
#y2k9 <- 300 + 750 + 500 + 750 + 6000 + 500 + 1350 + 8000 + 1000 + 5000 + 1980 + 75 + 10000 + 25000 + 10000
#y2k10 <- 300 + 750 + 600 + 750 + 6000 + 300 + 1500 + 8000 + 1500 + 3000 + 3500 + 75 + 10000 + 11500 + 30000 + 14000
#y2k11 <- 300 + 4000 + 750 + 750 + 5500 + 10000 + 750 + 3500 + 10000 + 1000 + 2000 + 1460 + 200 + 11000 + 36000 + 18000
#y2k12 <- 300 + 4000 + 7000 + 1000 + 300 + 5500 + 250 + 30000 + 1500 + 1875 + 6000 + 350 + 1500 + 1200 + 2250 + 200 + 12000 +6000 + 30000 + 22500
#y2k13 <- 380 + 3000 + 9000 + 1000 + 700 + 6000 + 40000 + 5000 + 2050 + 11000 + 3000 + 6000 + 1250 + 200 + 350 + 29000 + 25000 + 11000
#y2k14 <- 400 + 100 + 3000 + 1250 + 800 + 6000 + 70000 + 15000 + 2225 + 75 + 3000 + 5500 + 1500 + 2000 + 28000 + 25000 + 25000 + 11000
#y2k15 <- 400 + 100 + 3000 + 1500 + 1000 + 8000 + 15000 + 40000+ 2400 + 54000 +3000 + 8000 + 6000 + 1250 + 10000 + 27000 + 27000 + 10000
#y2k16 <- 400 + 3000 + 1750 + 2000 + 8000 + 15000 + 10000 + 2900 + 31000 + 3000 + 7500 + 5000 + 750 + 25000 + 29000 + 20000
#y2k17 <- 400 + 3000 + 3000 + 3000 + 8000 + 10000 + 5000 + 3417 + 19000 + 3000 + 18000 + 1000 + 250 + 10500 + 32500 + 12000 + 45000 + 30000
#y2k18 <- 400 + 32000 + 1500 + 3000 + 30000 + 200 + 30000 + 700 + 50 + 1000 + 3500 + 2000 + 35000 + 60000 + 35000

# Determining estimated Facebook accounts from removed content metrics on Twitter
#avg_twitter_posts <- (50.92 + 1969.34 + 50.16 + 4 + 549.67 + 79.74 + 137.82 + 378.49) / 8
# Possible outliers removed
#avg_twitter_posts_no <- (50.92 + 50.16 + 549.67 + 79.74 + 137.82 + 378.49) / 6
# Based on 207.8 posts per account
#fb17 <- 4400000 / avg_twitter_posts_no
#fb172 <- 4400000 / avg_twitter_posts
#fb18 <- 19300000 / avg_twitter_posts_no
#fb182 <- 19300000 / avg_twitter_posts
#fb19 <- 25500000 / avg_twitter_posts_no
#fb192 <- 25500000 /avg_twitter_posts
#ig19 <- 434000 / avg_twitter_posts_no
#ig192 <- 434000 / avg_twitter_posts

# Weighted Estimates of online activity per year (1998-2000 Omitted for only having one value each)
#on_2001 <- (633 * .6) + (50 * .4)
#on_2002 <- (2589 * .4) + (100 * .3) + (6 * .3)
#on_2003 <- (4544 * .4) + (500 * .3) + (11 * .3)
#on_2004 <- (6500 * .3) + (900 * .3) + (12 * .1) + (4000 * .3)
#on_2005 <- (1700 * .2) + (16 * .2) + (8000 * .6)
#on_2006 <- (11000 * .1) + (3000 * .2) + (58 * .2) + (23000 * .6) + (1000 * .1)
#on_2007 <- (1000 * .2) + (4700 * .1) + (88 * .1) + (43000 * .5) + (5000 * .1) 
#on_2008 <- (63000 * .8) + (5600 * .2)
#on_2009 <- (18000 * .6) + (120 + .2) + (6500 * .2)
#on_2010 <- (23000 * .5) + (7000 * .1) + (1000 *.1) + (10000 * .3)
#on_2011 <- (20000 * .4) + (20000 * .4) + (7650 * .1) + (1064 * .05) + (12 * .05)
#on_2012 <- (15000 * .3) + (30000 * .4) + (8200 * .1) + (2398 * .05) + (12 * .05)
#on_2013 <- (20000 * .2) + (40000 * .3) + (8800 * .05) + (2398 * .05) + (12 * .15) + (250 * .05) + (24448 * .2)
#on_2014 <- (10000 * .05) + (51000 * .25) + (9400 * .05) + (26 * .05) + (102000 * .4) + (170000 * .2)
#on_2015 <- (10000 * .05) + (10000 * .05) + (51000 * .2) + (33 * .05) + (78 * .05) + (210000 * .4) + (100000 * .2)
#on_2016 <- (500000 * .3) + (25543 * .2) + (963 * .05) + (66000 * .1) + (25050 * .1) + (15000 * .05) + (75000 * .2)
#on_2017 <- (500000 * .3) + (72060 * .3) + (272 * .05) + (15000 * .1) + (60000 * .1) + (50000 * .15)
#on_2018 <- (311602 * .15) + (94273 * .4) + (52 * .05) + (15000 * .1) + (66384 * .1) + (93000 * .15) + (1000 * .05)
