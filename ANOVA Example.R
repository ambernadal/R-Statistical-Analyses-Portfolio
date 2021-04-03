library(dplyr)
library(gplots)

dissdata <- read.csv('C:\\Users\\Amber Nadal\\Documents\\cisdata-merged2.csv')

################creating qualitative groups
#create z-scores for marital quality
mean(dissdata$mhqmi, na.rm=T)
sd(dissdata$mhqmi, na.rm=T)
dissdata$zmhqmi <- (((dissdata$mhqmi)-(6.235663))/(.9718749))
mean(dissdata$zmhqmi, na.rm=T)
summary(dissdata$zmhqmi)

#create z-scores for gratitude
mean(dissdata$mhgrat, na.rm=T)
sd(dissdata$mhgrat, na.rm=T)
dissdata$zmhgrat <- ((dissdata$mhgrat-(4.639865))/.5129881)
mean(dissdata$zmhgrat, na.rm=T)
summary(dissdata$zmhgrat)

#creating a z-score difference variable
dissdata$hzdiff <- dissdata$zmhgrat-dissdata$zmhqmi
summary(dissdata$hzdiff)

#creating groups of "deficient gratitude (relative to relationship quality)," "Appropriate," and "Excessive"
dissdata$hgratgroup <- ifelse(dissdata$hzdiff < -.1, 1,
                              ifelse(dissdata$hzdiff > .1, 3,
                                     2))
#making it qualitative
is.factor(dissdata$hgratgroup)
is.numeric(dissdata$hgratgroup)
dissdata$hgratgroup <- factor(dissdata$hgratgroup, labels = c("Deficient Gratitude", "Appropriate Gratitude", "Excessive Gratitude"))
is.factor(dissdata$hgratgroup)

#F-test ANOVA analysis of how these groups are related to commitment in a relationship
hpgwbiaov <- aov(mhpgwbi ~ hgratgroup, data = dissdata)
summary(hpgwbiaov)
#follow-up group comparisons
TukeyHSD(hpgwbiaov)
#plotting the groups in terms of level of commitment
plotmeans(hpgwbi ~ hgratgroup, data = dissdata,
          xlab = "Gratitude Group", ylab = "Psychological Well-being",
          main="Husband Psychological Well-being and Gratitude")
