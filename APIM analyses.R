library(lavaan)
library(semTools)

dissdata <- read.csv('C:\\Users\\Amber Nadal\\Documents\\cisdata-merged2.csv')

#creating z-score versions of the variables of interest pooled across husbands and wives
#negatively perceived couples exchanges
zdata1 <- dissdata$mhsrin
zdata2 <- dissdata$mwsrin
zdata <- rbind(zdata1,zdata2) #the data is in wide format, this step creates a long format version of the variables for pooling purposes
mean(zdata, na.rm=T)
sd(zdata, na.rm=T)

colnames(dissdata)
dissdata$zmhsrin <- ((dissdata$mhsrin-1.818)/1.011)
dissdata$zmwsrin <- ((dissdata$mwsrin-1.818)/1.011)
mean(dissdata$zmhsrin, na.rm=T)
mean(dissdata$zmwsrin, na.rm=T)

#positively perceived couples exchanges
zdata1 <- dissdata$mhsrip
zdata2 <- dissdata$mwsrip
zdata <- rbind(zdata1,zdata2)
mean(zdata, na.rm=T)
sd(zdata, na.rm=T)

dissdata$zmhsrip <- ((dissdata$mhsrip-4.979)/1.146)
dissdata$zmwsrip <- ((dissdata$mwsrip-4.979)/1.146)
mean(dissdata$zmhsrip, na.rm=T)
mean(dissdata$zmwsrip, na.rm=T)


#relationship satisfaction
zdata1 <- dissdata$mhqmi
zdata2 <- dissdata$mwqmi
zdata <- rbind(zdata1,zdata2)
mean(zdata, na.rm=T)
sd(zdata, na.rm=T)

dissdata$zmhqmi <- ((dissdata$mhqmi-6.177)/1.049)
dissdata$zmwqmi <- ((dissdata$mwqmi-6.177)/1.049)
mean(dissdata$zmhqmi, na.rm=T)
mean(dissdata$zmwqmi, na.rm=T)

#creating interaction variables and curvilinear variables for the analysis
dissdata$zhX <- dissdata$zmhsrip*dissdata$zmhsrin
dissdata$zwX <- dissdata$zmwsrip*dissdata$zmwsrin
dissdata$zhn2 <- dissdata$zmhsrin*dissdata$zmhsrin
dissdata$zwn2 <- dissdata$zmwsrin*dissdata$zmwsrin
dissdata$zhp2 <- dissdata$zmhsrip*dissdata$zmhsrip
dissdata$zwp2 <- dissdata$zmwsrip*dissdata$zmwsrip
mean(dissdata$mlength, na.rm=T)
sd(dissdata$mlength, na.rm=T)
dissdata$zmlength <- ((dissdata$mlength-15.545)/12.463)

##########################Examining correlations and descriptive statistics for the variables#############
min(dissdata$zmhsrip, na.rm=T)
max(dissdata$zmhsrip, na.rm=T)
mean(dissdata$zmhsrip, na.rm=T)

min(dissdata$zmhsrin, na.rm=T)
max(dissdata$zmhsrin, na.rm=T)
mean(dissdata$zmhsrin, na.rm=T)
cor.test(dissdata$zmhsrin, dissdata$zmhsrip, na.rm=T)

min(dissdata$zmhqmi, na.rm=T)
max(dissdata$zmhqmi, na.rm=T)
mean(dissdata$zmhqmi, na.rm=T)
cor.test(dissdata$zmhqmi, dissdata$zmhsrip, na.rm=T)
cor.test(dissdata$zmhqmi, dissdata$zmhsrin, na.rm=T)

min(dissdata$zmwsrip, na.rm=T)
max(dissdata$zmwsrip, na.rm=T)
mean(dissdata$zmwsrip, na.rm=T)
cor.test(dissdata$zmwsrip, dissdata$zmhsrip, na.rm=T)
cor.test(dissdata$zmwsrip, dissdata$zmhsrin, na.rm=T)
cor.test(dissdata$zmwsrip, dissdata$zmhqmi, na.rm=T)

min(dissdata$zmwsrin, na.rm=T)
max(dissdata$zmwsrin, na.rm=T)
mean(dissdata$zmwsrin, na.rm=T)
cor.test(dissdata$zmwsrin, dissdata$zmhsrip, na.rm=T)
cor.test(dissdata$zmwsrin, dissdata$zmhsrin, na.rm=T)
cor.test(dissdata$zmwsrin, dissdata$zmhqmi, na.rm=T)
cor.test(dissdata$zmwsrin, dissdata$zmwsrip, na.rm=T)

min(dissdata$zmwqmi, na.rm=T)
max(dissdata$zmwqmi, na.rm=T)
mean(dissdata$zmwqmi, na.rm=T)
cor.test(dissdata$zmwqmi, dissdata$zmhsrip, na.rm=T)
cor.test(dissdata$zmwqmi, dissdata$zmhsrin, na.rm=T)
cor.test(dissdata$zmwqmi, dissdata$zmhqmi, na.rm=T)
cor.test(dissdata$zmwqmi, dissdata$zmwsrip, na.rm=T)
cor.test(dissdata$zmwqmi, dissdata$zmwsrin, na.rm=T)

cor.test(dissdata$zmwsrip, dissdata$zmhsrip, na.rm=T)
cor.test(dissdata$zmwsrin, dissdata$zmhsrin, na.rm=T)
cor.test(dissdata$zmwqmi, dissdata$zmhqmi, na.rm=T)

#########################################Estimating and comparing APIM SEM models########################


#creating an actor-partner interdependence model including all predictors. Including length of marriage control which is unconvaried with the other predictors in order to theoretically identify the model.
full.model <- '
zmhqmi ~ zmhsrin + zmhsrip + zhX + zhn2 + zhp2 + zmlength + zmwsrin + zmwsrip + zwX + zwn2 + zwp2
zmwqmi ~ zmhsrin + zmhsrip + zhX + zhn2 + zhp2 + zmlength + zmwsrin + zmwsrip + zwX + zwn2 + zwp2
zmlength ~~ 0*zmhsrin
zmlength ~~ 0*zmhsrip
zmlength ~~ 0*zhX
zmlength ~~ 0*zmwsrin
zmlength ~~ 0*zmwsrip
zmlength ~~ 0*zwX
zmlength ~~ 0*zhn2
zmlength ~~ 0*zhp2
zmlength ~~ 0*zwn2
zmlength ~~ 0*zwp2
zmhqmi ~~ zmwqmi
zmhsrin ~~ zmhsrip
zmhsrin ~~ zhX
zmhsrin ~~ zhn2
zmhsrin ~~ zhp2
zmhsrin ~~ zmwsrin
zmhsrin ~~ zmwsrip
zmhsrin ~~ zwX
zmhsrin ~~ zwn2
zmhsrin ~~ zwp2
zmhsrip ~~ zhX
zmhsrip ~~ zhn2
zmhsrip ~~ zhp2
zmhsrip ~~ zmwsrin
zmhsrip ~~ zmwsrip
zmhsrip ~~ zwX
zmhsrip ~~ zwn2
zmhsrip ~~ zwp2
zhX ~~ zhn2
zhX ~~ zhp2
zhX ~~ zmwsrin
zhX ~~ zmwsrip
zhX ~~ zwX
zhX ~~ zwn2
zhX ~~ zwp2
zhn2 ~~ zhp2
zhn2 ~~ zmwsrin
zhn2 ~~ zmwsrip
zhn2 ~~ zwX
zhn2 ~~ zwn2
zhn2 ~~ zwp2
zhp2 ~~ zmwsrin
zhp2 ~~ zmwsrip
zhp2 ~~ zwX
zhp2 ~~ zwn2
zhp2 ~~ zwp2
zmwsrin ~~ zmwsrip
zmwsrin ~~ zwX
zmwsrin ~~ zwn2
zmwsrin ~~ zwp2
zmwsrip ~~ zwX
zmwsrip ~~ zwn2
zmwsrip ~~ zwp2
zwX ~~ zwn2
zwX ~~ zwp2
zwn2 ~~ zwp2
'

full.output <- sem(model = full.model,
                   data = dissdata)

summary(full.output,
        fit.measures = TRUE,
        rsquare = TRUE)

#this model is the same as the above, but sets the effects of the interactive terms on relationship satisfaction at 0, so that the fit of this model can be compared to the full model above.
curv.model <- '
zmhqmi ~ zmhsrin + zmhsrip + zhn2 + zhp2 + zmlength + zmwsrin + zmwsrip + zwn2 + zwp2
zmhqmi ~ 0*zhX
zmhqmi ~ 0*zwX
zmwqmi ~ zmhsrin + zmhsrip + zhn2 + zhp2 + zmlength + zmwsrin + zmwsrip + zwn2 + zwp2
zmwqmi ~ 0*zhX
zmwqmi ~ 0*zwX
zmlength ~~ 0*zmhsrin
zmlength ~~ 0*zmhsrip
zmlength ~~ 0*zhX
zmlength ~~ 0*zmwsrin
zmlength ~~ 0*zmwsrip
zmlength ~~ 0*zwX
zmlength ~~ 0*zhn2
zmlength ~~ 0*zhp2
zmlength ~~ 0*zwn2
zmlength ~~ 0*zwp2
zmhqmi ~~ zmwqmi
zmhsrin ~~ zmhsrip
zmhsrin ~~ zhX
zmhsrin ~~ zhn2
zmhsrin ~~ zhp2
zmhsrin ~~ zmwsrin
zmhsrin ~~ zmwsrip
zmhsrin ~~ zwX
zmhsrin ~~ zwn2
zmhsrin ~~ zwp2
zmhsrip ~~ zhX
zmhsrip ~~ zhn2
zmhsrip ~~ zhp2
zmhsrip ~~ zmwsrin
zmhsrip ~~ zmwsrip
zmhsrip ~~ zwX
zmhsrip ~~ zwn2
zmhsrip ~~ zwp2
zhX ~~ zhn2
zhX ~~ zhp2
zhX ~~ zmwsrin
zhX ~~ zmwsrip
zhX ~~ zwX
zhX ~~ zwn2
zhX ~~ zwp2
zhn2 ~~ zhp2
zhn2 ~~ zmwsrin
zhn2 ~~ zmwsrip
zhn2 ~~ zwX
zhn2 ~~ zwn2
zhn2 ~~ zwp2
zhp2 ~~ zmwsrin
zhp2 ~~ zmwsrip
zhp2 ~~ zwX
zhp2 ~~ zwn2
zhp2 ~~ zwp2
zmwsrin ~~ zmwsrip
zmwsrin ~~ zwX
zmwsrin ~~ zwn2
zmwsrin ~~ zwp2
zmwsrip ~~ zwX
zmwsrip ~~ zwn2
zmwsrip ~~ zwp2
zwX ~~ zwn2
zwX ~~ zwp2
zwn2 ~~ zwp2
'

curv.output <- sem(model = curv.model,
                   data = dissdata)

summary(curv.output,
        fit.measures = TRUE,
        rsquare = TRUE
)


#this model is the same as the above, but sets the effects of the curvilinear terms on relationship satisfaction at 0, so that the fit of this model can be compared to the full model and the curvilinear model above.
int.model <- '
zmhqmi ~ zmhsrin + zmhsrip + zhX + zmlength + zmwsrip + zmwsrin + zwX
zmhqmi ~ 0*zhn2
zmhqmi ~ 0*zhp2
zmhqmi ~ 0*zwn2
zmhqmi ~ 0*zwp2
zmwqmi ~ zmhsrin + zmhsrip + zhX + zmlength + zmwsrin + zmwsrip + zwX
zmwqmi ~ 0*zhn2
zmwqmi ~ 0*zhp2
zmwqmi ~ 0*zwn2
zmwqmi ~ 0*zwp2
zmlength ~~ 0*zmhsrin
zmlength ~~ 0*zmhsrip
zmlength ~~ 0*zhX
zmlength ~~ 0*zmwsrin
zmlength ~~ 0*zmwsrip
zmlength ~~ 0*zwX
zmlength ~~ 0*zhn2
zmlength ~~ 0*zhp2
zmlength ~~ 0*zwn2
zmlength ~~ 0*zwp2
zmhqmi ~~ zmwqmi
zmhsrin ~~ zmhsrip
zmhsrin ~~ zhX
zmhsrin ~~ zhn2
zmhsrin ~~ zhp2
zmhsrin ~~ zmwsrin
zmhsrin ~~ zmwsrip
zmhsrin ~~ zwX
zmhsrin ~~ zwn2
zmhsrin ~~ zwp2
zmhsrip ~~ zhX
zmhsrip ~~ zhn2
zmhsrip ~~ zhp2
zmhsrip ~~ zmwsrin
zmhsrip ~~ zmwsrip
zmhsrip ~~ zwX
zmhsrip ~~ zwn2
zmhsrip ~~ zwp2
zhX ~~ zhn2
zhX ~~ zhp2
zhX ~~ zmwsrin
zhX ~~ zmwsrip
zhX ~~ zwX
zhX ~~ zwn2
zhX ~~ zwp2
zhn2 ~~ zhp2
zhn2 ~~ zmwsrin
zhn2 ~~ zmwsrip
zhn2 ~~ zwX
zhn2 ~~ zwn2
zhn2 ~~ zwp2
zhp2 ~~ zmwsrin
zhp2 ~~ zmwsrip
zhp2 ~~ zwX
zhp2 ~~ zwn2
zhp2 ~~ zwp2
zmwsrin ~~ zmwsrip
zmwsrin ~~ zwX
zmwsrin ~~ zwn2
zmwsrin ~~ zwp2
zmwsrip ~~ zwX
zmwsrip ~~ zwn2
zmwsrip ~~ zwp2
zwX ~~ zwn2
zwX ~~ zwp2
zwn2 ~~ zwp2
'

int.output <- sem(model = int.model,
                  data = dissdata,
                  meanstructure = TRUE,
                  asymptotic=FALSE)

summary(int.output,
        fit.measures = TRUE,
        rsquare = TRUE
)



#this model is the same as the above, but sets the effects of the interactive terms and the curvilinear terms on relationship satisfaction at 0, so that the fit of this model can be compared to the full model, the curvilinear model, and the interactive model above.
lin.model <- '
zmhqmi ~ zmhsrin + zmhsrip + zmlength + zmwsrip + zmwsrin
zmhqmi ~ 0*zhn2
zmhqmi ~ 0*zhp2
zmhqmi ~ 0*zwn2
zmhqmi ~ 0*zwp2
zmhqmi ~ 0*zhX
zmhqmi ~ 0*zwX
zmwqmi ~ zmhsrin + zmhsrip + zmlength + zmwsrin + zmwsrip
zmwqmi ~ 0*zhn2
zmwqmi ~ 0*zhp2
zmwqmi ~ 0*zwn2
zmwqmi ~ 0*zwp2
zmwqmi ~ 0*zhX
zmwqmi ~ 0*zwX
zmlength ~~ 0*zmhsrin
zmlength ~~ 0*zmhsrip
zmlength ~~ 0*zhX
zmlength ~~ 0*zmwsrin
zmlength ~~ 0*zmwsrip
zmlength ~~ 0*zwX
zmlength ~~ 0*zhn2
zmlength ~~ 0*zhp2
zmlength ~~ 0*zwn2
zmlength ~~ 0*zwp2
zmhqmi ~~ zmwqmi
zmhsrin ~~ zmhsrip
zmhsrin ~~ zhX
zmhsrin ~~ zhn2
zmhsrin ~~ zhp2
zmhsrin ~~ zmwsrin
zmhsrin ~~ zmwsrip
zmhsrin ~~ zwX
zmhsrin ~~ zwn2
zmhsrin ~~ zwp2
zmhsrip ~~ zhX
zmhsrip ~~ zhn2
zmhsrip ~~ zhp2
zmhsrip ~~ zmwsrin
zmhsrip ~~ zmwsrip
zmhsrip ~~ zwX
zmhsrip ~~ zwn2
zmhsrip ~~ zwp2
zhX ~~ zhn2
zhX ~~ zhp2
zhX ~~ zmwsrin
zhX ~~ zmwsrip
zhX ~~ zwX
zhX ~~ zwn2
zhX ~~ zwp2
zhn2 ~~ zhp2
zhn2 ~~ zmwsrin
zhn2 ~~ zmwsrip
zhn2 ~~ zwX
zhn2 ~~ zwn2
zhn2 ~~ zwp2
zhp2 ~~ zmwsrin
zhp2 ~~ zmwsrip
zhp2 ~~ zwX
zhp2 ~~ zwn2
zhp2 ~~ zwp2
zmwsrin ~~ zmwsrip
zmwsrin ~~ zwX
zmwsrin ~~ zwn2
zmwsrin ~~ zwp2
zmwsrip ~~ zwX
zmwsrip ~~ zwn2
zmwsrip ~~ zwp2
zwX ~~ zwn2
zwX ~~ zwp2
zwn2 ~~ zwp2
'

lin.output <- sem(model = lin.model,
                  data = dissdata,
                  meanstructure = TRUE)
summary(lin.output,
        fit.measures = TRUE,
        rsquare = TRUE
)

#########################################simple slopes analysis#######################################

#creating "splits" of zmhsrip variable
dissdata$zmhsriplo = dissdata$zmhsrip + sd(dissdata$zmhsrip, na.rm=T)
dissdata$zmhsriphi = dissdata$zmhsrip - sd(dissdata$zmhsrip, na.rm=T)
dissdata$lozhX <- dissdata$zmhsrin*dissdata$zmhsriplo
dissdata$hizhX <- dissdata$zmhsrin*dissdata$zmhsriphi

dissdata$zmwsriplo = dissdata$zmwsrip + sd(dissdata$zmwsrip, na.rm=T)
dissdata$zmwsriphi = dissdata$zmwsrip - sd(dissdata$zmwsrip, na.rm=T)
dissdata$lozwX <- dissdata$zmwsrin*dissdata$zmwsriplo
dissdata$hizwX <- dissdata$zmwsrin*dissdata$zmwsriphi

#simples slopes anlaysis of husband actor interaction effect
#running analysis again with these new variables
lointss.model <- '
zmhqmi ~ zmhsrin + zmhsriplo + lozhX + zmlength + zmwsrip + zmwsrin + zwX
zmhqmi ~ 0*zhn2
zmhqmi ~ 0*zhp2
zmhqmi ~ 0*zwn2
zmhqmi ~ 0*zwp2
zmwqmi ~ zmhsrin + zmhsriplo + lozhX + zmlength + zmwsrin + zmwsrip + zwX
zmwqmi ~ 0*zhn2
zmwqmi ~ 0*zhp2
zmwqmi ~ 0*zwn2
zmwqmi ~ 0*zwp2
zmlength ~~ 0*zmhsrin
zmlength ~~ 0*zmhsriplo
zmlength ~~ 0*lozhX
zmlength ~~ 0*zmwsrin
zmlength ~~ 0*zmwsrip
zmlength ~~ 0*zwX
zmlength ~~ 0*zhn2
zmlength ~~ 0*zhp2
zmlength ~~ 0*zwn2
zmlength ~~ 0*zwp2
zmhqmi ~~ zmwqmi
zmhsrin ~~ zmhsriplo
zmhsrin ~~ lozhX
zmhsrin ~~ zhn2
zmhsrin ~~ zhp2
zmhsrin ~~ zmwsrin
zmhsrin ~~ zmwsrip
zmhsrin ~~ zwX
zmhsrin ~~ zwn2
zmhsrin ~~ zwp2
zmhsriplo ~~ lozhX
zmhsriplo ~~ zhn2
zmhsriplo ~~ zhp2
zmhsriplo ~~ zmwsrin
zmhsriplo ~~ zmwsrip
zmhsriplo ~~ zwX
zmhsriplo ~~ zwn2
zmhsriplo ~~ zwp2
lozhX ~~ zhn2
lozhX ~~ zhp2
lozhX ~~ zmwsrin
lozhX ~~ zmwsrip
lozhX ~~ zwX
lozhX ~~ zwn2
lozhX ~~ zwp2
zhn2 ~~ zhp2
zhn2 ~~ zmwsrin
zhn2 ~~ zmwsrip
zhn2 ~~ zwX
zhn2 ~~ zwn2
zhn2 ~~ zwp2
zhp2 ~~ zmwsrin
zhp2 ~~ zmwsrip
zhp2 ~~ zwX
zhp2 ~~ zwn2
zhp2 ~~ zwp2
zmwsrin ~~ zmwsrip
zmwsrin ~~ zwX
zmwsrin ~~ zwn2
zmwsrin ~~ zwp2
zmwsrip ~~ zwX
zmwsrip ~~ zwn2
zmwsrip ~~ zwp2
zwX ~~ zwn2
zwX ~~ zwp2
zwn2 ~~ zwp2
'

lointss.output <- sem(model = lointss.model,
                  data = dissdata,
                  meanstructure = TRUE)
summary(lointss.output,
        fit.measures = TRUE,
        rsquare = TRUE
)

hiintss.model <- '
zmhqmi ~ zmhsrin + zmhsriphi + hizhX + zmlength + zmwsrip + zmwsrin + zwX
zmhqmi ~ 0*zhn2
zmhqmi ~ 0*zhp2
zmhqmi ~ 0*zwn2
zmhqmi ~ 0*zwp2
zmwqmi ~ zmhsrin + zmhsriphi + hizhX + zmlength + zmwsrin + zmwsrip + zwX
zmwqmi ~ 0*zhn2
zmwqmi ~ 0*zhp2
zmwqmi ~ 0*zwn2
zmwqmi ~ 0*zwp2
zmlength ~~ 0*zmhsrin
zmlength ~~ 0*zmhsriphi
zmlength ~~ 0*hizhX
zmlength ~~ 0*zmwsrin
zmlength ~~ 0*zmwsrip
zmlength ~~ 0*zwX
zmlength ~~ 0*zhn2
zmlength ~~ 0*zhp2
zmlength ~~ 0*zwn2
zmlength ~~ 0*zwp2
zmhqmi ~~ zmwqmi
zmhsrin ~~ zmhsriphi
zmhsrin ~~ hizhX
zmhsrin ~~ zhn2
zmhsrin ~~ zhp2
zmhsrin ~~ zmwsrin
zmhsrin ~~ zmwsrip
zmhsrin ~~ zwX
zmhsrin ~~ zwn2
zmhsrin ~~ zwp2
zmhsriphi ~~ hizhX
zmhsriphi ~~ zhn2
zmhsriphi ~~ zhp2
zmhsriphi ~~ zmwsrin
zmhsriphi ~~ zmwsrip
zmhsriphi ~~ zwX
zmhsriphi ~~ zwn2
zmhsriphi ~~ zwp2
hizhX ~~ zhn2
hizhX ~~ zhp2
hizhX ~~ zmwsrin
hizhX ~~ zmwsrip
hizhX ~~ zwX
hizhX ~~ zwn2
hizhX ~~ zwp2
zhn2 ~~ zhp2
zhn2 ~~ zmwsrin
zhn2 ~~ zmwsrip
zhn2 ~~ zwX
zhn2 ~~ zwn2
zhn2 ~~ zwp2
zhp2 ~~ zmwsrin
zhp2 ~~ zmwsrip
zhp2 ~~ zwX
zhp2 ~~ zwn2
zhp2 ~~ zwp2
zmwsrin ~~ zmwsrip
zmwsrin ~~ zwX
zmwsrin ~~ zwn2
zmwsrin ~~ zwp2
zmwsrip ~~ zwX
zmwsrip ~~ zwn2
zmwsrip ~~ zwp2
zwX ~~ zwn2
zwX ~~ zwp2
zwn2 ~~ zwp2
'

hiintss.output <- sem(model = hiintss.model,
                      data = dissdata,
                      meanstructure = TRUE)
summary(hiintss.output,
        fit.measures = TRUE,
        rsquare = TRUE
)


#simple slopes of wife actor moderation effect & simple slopes of cross-partner effect of wife positive and negative exchanges on husband relationship satisfaction
wloint.model <- '
zmhqmi ~ zmhsrin + zmhsrip + zhX + zmlength + zmwsriplo + zmwsrin + lozwX
zmhqmi ~ 0*zhn2
zmhqmi ~ 0*zhp2
zmhqmi ~ 0*zwn2
zmhqmi ~ 0*zwp2
zmwqmi ~ zmhsrin + zmhsrip + zhX + zmlength + zmwsriplo + zmwsrin + lozwX
zmwqmi ~ 0*zhn2
zmwqmi ~ 0*zhp2
zmwqmi ~ 0*zwn2
zmwqmi ~ 0*zwp2
zmlength ~~ 0*zmhsrin
zmlength ~~ 0*zmhsrip
zmlength ~~ 0*zhX
zmlength ~~ 0*zmwsrin
zmlength ~~ 0*zmwsriplo
zmlength ~~ 0*lozwX
zmlength ~~ 0*zhn2
zmlength ~~ 0*zhp2
zmlength ~~ 0*zwn2
zmlength ~~ 0*zwp2
zmhqmi ~~ zmwqmi
zmhsrin ~~ zmhsrip
zmhsrin ~~ zhX
zmhsrin ~~ zhn2
zmhsrin ~~ zhp2
zmhsrin ~~ zmwsrin
zmhsrin ~~ zmwsriplo
zmhsrin ~~ lozwX
zmhsrin ~~ zwn2
zmhsrin ~~ zwp2
zmhsrip ~~ zhX
zmhsrip ~~ zhn2
zmhsrip ~~ zhp2
zmhsrip ~~ zmwsrin
zmhsrip ~~ zmwsriplo
zmhsrip ~~ lozwX
zmhsrip ~~ zwn2
zmhsrip ~~ zwp2
zhX ~~ zhn2
zhX ~~ zhp2
zhX ~~ zmwsrin
zhX ~~ zmwsriplo
zhX ~~ lozwX
zhX ~~ zwn2
zhX ~~ zwp2
zhn2 ~~ zhp2
zhn2 ~~ zmwsrin
zhn2 ~~ zmwsriplo
zhn2 ~~ lozwX
zhn2 ~~ zwn2
zhn2 ~~ zwp2
zhp2 ~~ zmwsrin
zhp2 ~~ zmwsriplo
zhp2 ~~ lozwX
zhp2 ~~ zwn2
zhp2 ~~ zwp2
zmwsrin ~~ zmwsriplo
zmwsrin ~~ lozwX
zmwsrin ~~ zwn2
zmwsrin ~~ zwp2
zmwsriplo ~~ lozwX
zmwsriplo ~~ zwn2
zmwsriplo ~~ zwp2
lozwX ~~ zwn2
lozwX ~~ zwp2
zwn2 ~~ zwp2
'

wloint.output <- sem(model = wloint.model,
                  data = dissdata,
                  meanstructure = TRUE)


summary(wloint.output,
        fit.measures = TRUE,
        rsquare = TRUE
)

whiint.model <- '
zmhqmi ~ zmhsrin + zmhsrip + zhX + zmlength + zmwsriphi + zmwsrin + hizwX
zmhqmi ~ 0*zhn2
zmhqmi ~ 0*zhp2
zmhqmi ~ 0*zwn2
zmhqmi ~ 0*zwp2
zmwqmi ~ zmhsrin + zmhsrip + zhX + zmlength + zmwsriphi + zmwsrin + hizwX
zmwqmi ~ 0*zhn2
zmwqmi ~ 0*zhp2
zmwqmi ~ 0*zwn2
zmwqmi ~ 0*zwp2
zmlength ~~ 0*zmhsrin
zmlength ~~ 0*zmhsrip
zmlength ~~ 0*zhX
zmlength ~~ 0*zmwsrin
zmlength ~~ 0*zmwsriphi
zmlength ~~ 0*hizwX
zmlength ~~ 0*zhn2
zmlength ~~ 0*zhp2
zmlength ~~ 0*zwn2
zmlength ~~ 0*zwp2
zmhqmi ~~ zmwqmi
zmhsrin ~~ zmhsrip
zmhsrin ~~ zhX
zmhsrin ~~ zhn2
zmhsrin ~~ zhp2
zmhsrin ~~ zmwsrin
zmhsrin ~~ zmwsriphi
zmhsrin ~~ hizwX
zmhsrin ~~ zwn2
zmhsrin ~~ zwp2
zmhsrip ~~ zhX
zmhsrip ~~ zhn2
zmhsrip ~~ zhp2
zmhsrip ~~ zmwsrin
zmhsrip ~~ zmwsriphi
zmhsrip ~~ hizwX
zmhsrip ~~ zwn2
zmhsrip ~~ zwp2
zhX ~~ zhn2
zhX ~~ zhp2
zhX ~~ zmwsrin
zhX ~~ zmwsriphi
zhX ~~ hizwX
zhX ~~ zwn2
zhX ~~ zwp2
zhn2 ~~ zhp2
zhn2 ~~ zmwsrin
zhn2 ~~ zmwsriphi
zhn2 ~~ hizwX
zhn2 ~~ zwn2
zhn2 ~~ zwp2
zhp2 ~~ zmwsrin
zhp2 ~~ zmwsriphi
zhp2 ~~ hizwX
zhp2 ~~ zwn2
zhp2 ~~ zwp2
zmwsrin ~~ zmwsriphi
zmwsrin ~~ hizwX
zmwsrin ~~ zwn2
zmwsrin ~~ zwp2
zmwsriphi ~~ hizwX
zmwsriphi ~~ zwn2
zmwsriphi ~~ zwp2
hizwX ~~ zwn2
hizwX ~~ zwp2
zwn2 ~~ zwp2
'

whiint.output <- sem(model = whiint.model,
                  data = dissdata,
                  meanstructure = TRUE)

summary(whiint.output,
        fit.measures = TRUE,
        rsquare = TRUE
)