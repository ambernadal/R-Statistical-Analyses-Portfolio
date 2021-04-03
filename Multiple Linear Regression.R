library(lavaan)
dissdata <- read.csv('C:\\Users\\Amber Nadal\\Documents\\cisdata-merged2.csv')

################predicting hubsnad health by his wife's health, the length of time married, and his
#income and education

mhhp <- lm(mhhp ~ mwhp + hmlength + hincome + hedu, data=dissdata)
summary(mhhp)
confint(mhhp, level=0.95)


#regression plots for the predictors with standard errors visualized (grey bands);
#NOTE: these do not control for the influence of the other predictors.
ggplot(dissdata, aes(x=mwhp, y=mhhp)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, color='#2C3E50')

ggplot(dissdata, aes(x=hmlength, y=mhhp)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, color='#2C3E50')

ggplot(dissdata, aes(x=hincome, y=mhhp)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, color='#2C3E50')

ggplot(dissdata, aes(x=hedu, y=mhhp)) + 
  geom_point(color='#2980B9', size = 4) + 
  geom_smooth(method=lm, color='#2C3E50')
