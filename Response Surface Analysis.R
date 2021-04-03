data <- read.csv("C:\\Users\\Amber Nadal\\Desktop\\Circumplex Analyses\\Data.csv")
data2 <- read.csv("C:\\Users\\Amber Nadal\\Desktop\\CIrcumplex Analyses\\wave1rawdata.csv")
data3 <- data2[, c("responseid","Q321")]
finaldata <- merge(x = data, y = data3, by = "responseid", all = TRUE)
print(finaldata[,c("patience", "courage", "Q321")])
library(moments)
library(RSA)
library(psych)
library(dplyr)

#IN THIS CODE, WE WILL BE EXAMINING HOW CONGRUENCES IN PATIENCE AND COURAGE ARE RELATED TO VICES.
#THEORY MAINTAINS THAT VIRTUES EXIST ALONG A CONTINUUM BETWEEN VICES OF DEFICIENTY AND EXCESS, AND THAT
#VIRTUES MAY OPERATE AS CHECK AND BALANCES TO AVOID THE EXPRESSION OF VICE. FOR INSANCE, CONSIDER THIS 
#CONTIUUM: APATHY -> <- PATIENCE -> <- COURAGE -> <- RECKLESSNESS
#WE WILL HERE EXAMINE WHETHER APATHY AND RECKLESSNESS ARE LOWEST WHEN THERE IS CONGRUENCE BETWEEN
#PATIENCE AND COURAGE

####THIS FIRST SECTION IS DATA PREP/CLEANING, SCROLL DOWN FOR RSA ANALYSES####
#checking ranges for the variables that will be averaged for "patience"
print(data$patience)
min(data$patience, na.rm = TRUE)
max(data$patience, na.rm = TRUE)
print(data$coolhead)
min(data$coolhead, na.rm = TRUE)
max(data$coolhead, na.rm = TRUE)
print(data$collected)
min(data$collected, na.rm = TRUE)
max(data$collected, na.rm = TRUE)
print(data$forbear)
min(data$forbear, na.rm = TRUE)
max(data$forbear, na.rm = TRUE)
#checking alphas to make aure it makes sense to group them
virpatience <- data[c('patience','coolhead','collected','forbear')]
psych::alpha(virpatience)
#dropping forbearing because the alpha is too low
virpatience <-data[c('patience','coolhead','collected')]
psych::alpha(virpatience)
#alpha of patience, coolhead, and collected is .66; this is still lower than we want, but we'll continue for the example
data$virtuepatience <- rowMeans(virpatience, na.rm=TRUE)
print(data$virtuepatience)
summary(data$virtuepatience)
scatter.smooth(data$virtuepatience)

#doing the same process of checking and creating an aggregated variable for the vice of excessive patience
print(data$passiv)
min(data$passiv, na.rm = TRUE)
max(data$passiv, na.rm = TRUE)
print(data$apathy)
min(data$apathy, na.rm = TRUE)
max(data$apathy, na.rm = TRUE)
print(data$restrain)
min(data$restrain, na.rm = TRUE)
max(data$restrain, na.rm = TRUE)
print(data$unenthus)
min(data$unenthus, na.rm = TRUE)
max(data$unenthus, na.rm = TRUE)
print(data$irresol)
min(data$irresol, na.rm = TRUE)
max(data$irresol, na.rm = TRUE)
print(data$detach)
min(data$detach, na.rm = TRUE)
max(data$detach, na.rm = TRUE)
#checking the alphas
vicpatience <- data[c('passiv','apathy','restrain','unenthus','irresol','detach')]
psych::alpha(vicpatience)
vicpatience <- data[c('passiv','apathy','unenthus','irresol','detach')]
psych::alpha(vicpatience)
#alpha goes down if we drop more, .67; still lower than we want, but will contrinue for the example
data$vicepatience <- rowMeans(vicpatience, na.rm=TRUE)
print(data$vicepatience)
summary(data$vicepatience)
scatter.smooth(data$virtuepatience, data$vicepatience)

#same process for the virtue of courage
print(data$courage)
min(data$courage, na.rm = TRUE)
max(data$courage, na.rm = TRUE)
print(data$brave)
min(data$brave, na.rm = TRUE)
max(data$brave, na.rm = TRUE)
print(data$convict)
min(data$convict, na.rm = TRUE)
max(data$convict, na.rm = TRUE)
print(data$bold)
min(data$bold, na.rm = TRUE)
max(data$bold, na.rm = TRUE)
#alphas
vircourage <- data[c('courage','brave','convict','bold')]
psych::alpha(vircourage)
vircourage <-data[c('courage','convict','bold')]
psych::alpha(vircourage)
#alpha of courage, convicted, and bold is .65; lower than ideal
data$virtuecourage <- rowMeans(vircourage, na.rm=TRUE)
print(data$virtuecourage)
summary(data$virtuecourage)

#vice of excessive courage
print(data$reckless)
min(data$reckless, na.rm = TRUE)
max(data$reckless, na.rm = TRUE)
print(data$imprud)
min(data$imprud, na.rm = TRUE)
max(data$imprud, na.rm = TRUE)
print(data$impulsive)
min(data$impulsive, na.rm = TRUE)
max(data$impulsive, na.rm = TRUE)
viccourage <- data[c('reckless','imprud','impulsive')]
psych::alpha(viccourage)
#alpha is .7
colnames(data)
data$vicecourage <- rowMeans(viccourage, na.rm=TRUE)
print(data$vicecourage)
summary(data$vicecourage)
scatter.smooth(data$virtuecourage, data$vicecourage)

#going to fence outliers at + or - 1.5 IQRs from the mean
summary(data$virtuepatience)
#2.5 lower fence
#6.00 upper fence
min(data$virtuepatience, na.rm=TRUE)
data$virtuepatience <- ifelse(data$virtuepatience <= 2.5, 2.5,
                              data$virtuepatience)
min(data$virtuepatience, na.rm=TRUE)
max(data$virtuepatience, na.rm=TRUE)
hist(data$virtuepatience)
skewness(data$virtuepatience, na.rm=TRUE)

summary(data$vicepatience)
# lower fence
#5.4 upper fence
min(data$vicepatience, na.rm=TRUE)
data$vicepatience <- ifelse(data$vicepatience >= 5.4, 5.4,
                            data$vicepatience)
min(data$vicepatience, na.rm=TRUE)
max(data$vicepatience, na.rm=TRUE)
hist(data$vicepatience)
skewness(data$vicepatience, na.rm=TRUE)

summary(data$virtuecourage)
# lower fence
# upper fence
min(data$virtuecourage, na.rm=TRUE)
max(data$virtuecourage, na.rm=TRUE)
hist(data$virtuecourage)
skewness(data$virtuecourage, na.rm=TRUE)

summary(data$vicecourage)
# lower fence
#4.5 upper fence
min(data$vicecourage, na.rm=TRUE)
data$vicecourage <- ifelse(data$vicecourage >= 4.5, 4.5,
                           data$vicecourage)
min(data$vicecourage, na.rm=TRUE)
max(data$vicecourage, na.rm=TRUE)
hist(data$vicecourage)
skewness(data$vicecourage, na.rm=TRUE)

#examining for bivariate outliers
scatter.smooth(data$virtuepatience, data$virtuecourage)
scatter.smooth(data$vicepatience, data$vicecourage)
scatter.smooth(data$virtuepatience, data$vicepatience)
scatter.smooth(data$virtuecourage, data$vicecourage)

##################################################RSA ANALYSES BEGIN HERE###########################
mvicepatience <-  RSA(
  vicepatience ~ virtuepatience*virtuecourage,
  model = c("full", "onlyx2", "onlyy2", "IA", "RRCA", "RRCL"),
  control.variables = c("vicecourage","responseid"),
  data = data,
  center = "pooled",
  na.rm = TRUE,
  missing = "fiml",
  out.rm = FALSE
)
compare(mvicepatience)
summary(mvicepatience, model="RRCA")
plot(mvicepatience, model="RRCA")
plot(mvicepatience, model="RRCA", type="c")


mvicecourage <-  RSA(
  vicecourage ~ virtuepatience*virtuecourage,
  model = c("full", "onlyx2", "onlyy2", "IA", "RRCA", "RRCL"),
  control.variables = c("vicepatience", "responseid"),
  data = data,
  center = "pooled",
  na.rm = TRUE,
  missing = "fiml",
  out.rm = FALSE
)
compare(mvicecourage)
summary(mvicecourage, model="RRCA")
plot(mvicecourage, model="RRCA")
plot(mvicecourage, model="RRCA", type="c")
