library(psych)
library(ggplot2)

dissdata <- read.csv('C:\\Users\\Amber Nadal\\Documents\\cisdata-merged2.csv')

################comparing overall sexual satisfaction of husbands and wives

t.test(dissdata$mwqmi, dissdata$mhqmi, na.rm=T)

relsat <- dissdata[c("mwqmi", "mhqmi")]
boxplot(relsat,
        data=airquality,
        main="Comparing Relationship Satisfaction 
        of Wives and Husbands",
        names=c("Wives", "Husbands"),
        ylab="Relationship Satisfaction",
        col="orange",
        border="brown"
)
