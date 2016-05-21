MAINPATH = "C:\\Users\\Romanos\\Projects\\rworkspace\\"
SUBPATH = "TheAnalyticsEdge\\"

setwd(MAINPATH + SUBPATH + "Unit1")
WHO = read.csv("WHO.csv")
str(WHO)
summary(WHO)
WHO_Europe = subset(WHO, Region == "Europe")
str(WHO_Europe)
write.csv(WHO_Europe, "WHO_Europe.csv")
rm(WHO_Europe)
ls()
WHO$Under15
mean(WHO$Under15)
sd(WHO$Under15)
summary(WHO$Under15)
which.min(WHO$Under15)
WHO$Country[86]
which.max(WHO$Under15)
WHO$Country[124]
plot(WHO$GNI, WHO$FertilityRate)
Outliers = subset(WHO, GNI > 10000 & FertilityRate > 2.5)
nr(Outliers)
Outliers[c("Country", "GNI", "FertilityRate")]
hist(WHO$CellularSubscribers)
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="LifeExpectancy")
boxplot(WHO$LifeExpectancy ~ WHO$Region, xlab="", ylab="LifeExpectancy", main="Life Expectancy of Countries by Region")
tapply(WHO$Over60, WHO$Region, mean)
tapply(WHO$LiteracyRate, WHO$Region, min)
tapply(WHO$LiteracyRate, WHO$Region, min, na.rm=TRUE)
tapply(WHO$ChildMortality, WHO$Region, mean)
min(tapply(WHO$ChildMortality, WHO$Region, mean))
