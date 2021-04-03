dissdata <- read.csv('C:\\Users\\Amber Nadal\\Documents\\cisdata-merged2.csv')
#checking to make sure the file loaded correctly
print(dissdata)
#loading packages we'll use for cleaning
library(dplyr)
library(psych)
library(moments)

#coding -999 as missing (-999 = missing code from SPSS) and aggregating
dissdata$hgrat2 <- ifelse(dissdata$hgrat2==-999, NA,
                          dissdata$hgrat2)
dissdata$hgrat6 <- ifelse(dissdata$hgrat6==-999, NA,
                          dissdata$hgrat6)
dissdata$hgrat21 <- ifelse(dissdata$hgrat21==-999, NA,
                           dissdata$hgrat21)
#assessing the alpha to make sure creating a mean of the variables is appropriate
mhgrat <- dissdata[c("hgrat2", "hgrat6", "hgrat21")]
psych::alpha(mhgrat)
#creating a mean of gratitude variable
dissdata$mhgrat <- rowMeans(mhgrat, na.rm=TRUE)
#fencint outliers at + or - 1.5 interquartile ranges from the mean
summary(dissdata$mhgrat)
#3.33 lower fence
#6.00 upper fence
min(dissdata$mhgrat, na.rm=TRUE)
dissdata$mhgrat <- ifelse(dissdata$mhgrat <= 3.33, 3.33,
                          dissdata$mhgrat)
#double-checking the ranges and visualizing the distribution to look for anything out of sorts
min(dissdata$mhgrat, na.rm=TRUE)
max(dissdata$mhgrat, na.rm=TRUE)
hist(dissdata$mhgrat)

skewness(dissdata$mhgrat, na.rm=TRUE)
#this variable is substantially skewed even after fencing outliers

#Creating a transformed version of the gratitude variable
#step 1: reflect the scores, and add a constant so the lowest number is 1
dissdata$thgrat1 <- (dissdata$mhgrat*-1+6)
min(dissdata$thgrat1, na.rm=TRUE)
max(dissdata$thgrat1, na.rm=TRUE)
#step 2: apply the transformation
dissdata$thgrat2 <- log10(dissdata$thgrat1)
min(dissdata$thgrat2, na.rm=TRUE)
max(dissdata$thgrat2, na.rm=TRUE)
#step 3: reflect the scores again to restore the original ordering
dissdata$thgrat3 <- (dissdata$thgrat2*-1)
min(dissdata$thgrat3, na.rm=TRUE)
max(dissdata$thgrat3, na.rm=TRUE)
#step 4: I am opting to add a constant so that the interpretation is easier later on
dissdata$thgrat <- (dissdata$thgrat3+1)
min(dissdata$thgrat, na.rm=TRUE)
max(dissdata$thgrat, na.rm=TRUE)
#now compare skew and hist
skewness(dissdata$thgrat, na.rm=TRUE)
hist(dissdata$thgrat)



#coding -999 as missing (-999 = missing code from SPSS) and aggregating
dissdata$hcom1 <- ifelse(dissdata$hcom1==-999, NA,
                         dissdata$hcom1)
dissdata$hcom2 <- ifelse(dissdata$hcom2==-999, NA,
                         dissdata$hcom2)
dissdata$hcom3 <- ifelse(dissdata$hcom3==-999, NA,
                         dissdata$hcom3)
dissdata$hcom4 <- ifelse(dissdata$hcom4==-999, NA,
                         dissdata$hcom4)
dissdata$hcom5 <- ifelse(dissdata$hcom5==-999, NA,
                         dissdata$hcom5)
#reverse-scoring items which are negatively worded
dissdata$hcom1r <- ((dissdata$hcom1*(-1))+6)
dissdata$hcom2r <- ((dissdata$hcom2*(-1))+6)
dissdata$hcom3r <- ((dissdata$hcom3*(-1))+6)
dissdata$hcom4r <- ((dissdata$hcom4*(-1))+6)
dissdata$hcom5r <- ((dissdata$hcom5*(-1))+6)
#assessing the alpha to make sure creating a mean of the variables is appropriate
mhcom <- dissdata[c("hcom1", "hcom2", "hcom3", "hcom4", "hcom5")]
psych::alpha(mhcom)
#creating mean variable
dissdata$mhcom <- rowMeans(mhcom, na.rm=TRUE)
#fencing outliers at + or - 1.5 IQRs from the mean
summary(dissdata$mhcom)
#2.0 lower fence
# upper fence
min(dissdata$mhcom, na.rm=TRUE)
dissdata$mhcom <- ifelse(dissdata$mhcom <= 2.0, 2.0,
                         dissdata$mhcom)
min(dissdata$mhcom, na.rm=TRUE)
max(dissdata$mhcom, na.rm=TRUE)
hist(dissdata$mhcom)

skewness(dissdata$mhcom, na.rm=TRUE)
#still very skewed

#step 1: reflect the scores, and add a constant so the lowest number is 1
dissdata$thcom1 <- (dissdata$mhcom*-1+6)
min(dissdata$thcom1, na.rm=TRUE)
max(dissdata$thcom1, na.rm=TRUE)
#step 2: apply the transformation
dissdata$thcom2 <- log10(dissdata$thcom1)
min(dissdata$thcom2, na.rm=TRUE)
max(dissdata$thcom2, na.rm=TRUE)
#step 3: reflect the scores again to restore the original ordering
dissdata$thcom3 <- (dissdata$thcom2*-1)
min(dissdata$thcom3, na.rm=TRUE)
max(dissdata$thcom3, na.rm=TRUE)
#step 4: I am opting to add a constant so that the interpretation is easier later on
dissdata$thcom <- (dissdata$thcom3+1)
min(dissdata$thcom, na.rm=TRUE)
max(dissdata$thcom, na.rm=TRUE)
#now compare skew and hist
skewness(dissdata$thcom, na.rm=TRUE)
#visualize new distribution
hist(dissdata$thcom)


#assess and visualiza bivariate correlations & compare with untransformed vars
cor.test(dissdata$mhgrat, dissdata$mhcom, na.rm=T)
scatter.smooth(dissdata$mhgrat, dissdata$mhcom, na.rm=T)

cor.test(dissdata$thgrat, dissdata$thcom, na.rm=T)
scatter.smooth(dissdata$thgrat, dissdata$thcom, na.rm=T)
