setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit1 - An Introduction to Analytics")
# Motor Vehicles 
DATA = read.csv("data\\mvtWeek1.csv")
table(DATA$Arrest)
max(DATA$ID)
min(DATA$Beat)
table(DATA$LocationDescription == "ALLEY")

DateConvert = as.Date(strptime(DATA$Date, "%m/%d/%y %H:%M"))
DATA$Date = DateConvert
DATA$Month = months(DateConvert)
DATA$Weekday = weekdays(DateConvert)
tapply(DATA$Arrest, DATA$Month, table)

date = as.Date(DATA$Date, "%m/%d/%Y")
DATA$Year = as.numeric(format(date, "%Y"))

sort(table(DATA$LocationDescription))
table(DATA$LocationDescription=="STREET" | DATA$LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | DATA$LocationDescription=="ALLEY" | DATA$LocationDescription=="GAS STATION" | DATA$LocationDescription=="DRIVEWAY - RESIDENTIAL")

Top5 = subset(DATA, LocationDescription=="STREET" | LocationDescription=="PARKING LOT/GARAGE(NON.RESID.)" | LocationDescription=="ALLEY" | LocationDescription=="GAS STATION" | LocationDescription=="DRIVEWAY - RESIDENTIAL")
Top5$LocationDescription = factor(Top5$LocationDescription)


# Stocks
IBM = read.csv("data\\IBMStock.csv")
GE = read.csv("data\\GEStock.csv")
CocaCola = read.csv("data\\CocaColaStock.csv")
ProcterGamble = read.csv("data\\ProcterGambleStock.csv")
Boeing = read.csv("data\\BoeingStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")

plot(CocaCola$Date, CocaCola$StockPrice, xlab="Date", ylab="Stockprice", type="l")
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col="red")
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-01-01")), lwd=2)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col="green")
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col="orange")
lines(GE$Date[301:432], GE$StockPrice[301:432], col="yellow")
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col="black")
abline(v=as.Date(c("1997-09-01")), lwd=2)
abline(v=as.Date(c("1997-11-01")), lwd=2)
abline(v=as.Date(c("2004-01-01")), lwd=2)
abline(v=as.Date(c("2005-12-31")), lwd=2)
abline(v=as.Date(c("2005-01-01")), lwd=2)

tapply(IBM$StockPrice, months(IBM$Date), mean)
mean(IBM$StockPrice)
max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))
max(tapply(GE$StockPrice, months(GE$Date), mean))

tapply(CocaCola$StockPrice, months(CocaCola$Date), mean)
tapply(GE$StockPrice, months(GE$Date), mean)


# Demographics and Employment
CPS = read.csv("data\\CPSData.csv")
summary(CPS$Industry)
sort(table(CPS$State))
tapply(CPS$Race, CPS$Hispanic, table)
summary(CPS)
is.na(CPS$Married)
table(CPS$Region, is.na(CPS$Married))
table(CPS$State, is.na(CPS$MetroAreaCode))
tapply(is.na(CPS$MetroAreaCode), CPS$State, mean)
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean))

MetroAreaMap = read.csv("data\\MetroAreaCodes.csv")
CountryMap = read.csv("data\\CountryCodes.csv")
CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
sort(tapply(CPS$Hispanic, CPS$MetroArea, mean))
sort(tapply(CPS$Race == "Asian", CPS$MetroArea, mean))
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm=TRUE))

CPS = merge(CPS, CountryMap, by.x="CountryOfBirthCode", by.y="Code", all.x=TRUE)

sort(tapply(CPS$Country == "India", CPS$MetroArea, sum, na.rm=TRUE))
sort(tapply(CPS$Country == "Brazil", CPS$MetroArea, sum, na.rm=TRUE))



# Optional HW
POLL = read.csv("data\\AnonymityPoll.csv")
table(POLL$Sex, POLL$Region)
table(POLL$State, POLL$Region)
table(POLL$Internet, POLL$Smartphone)

limited = subset(POLL, Smartphone == 1 | Internet == 1)
table(limited$Info.On.Internet)
table(limited$Worry.About.Info)
table(limited$Anonymity.Possible)
hist(limited$Age)
plot(limited$Age, limited$Info.On.Internet)
table(limited$Age, limited$Info.On.Internet)
plot(jitter(limited$Age), jitter(limited$Info.On.Internet))
tapply(limited$Info.On.Internet, limited$Smartphone, summary)


