USDA = read.csv("USDA.csv")
str(USDA)
summary(USDA)

which.max(USDA$Sodium)
names(USDA)
USDA$Description[265]
HighSodium = subset(USDA, Sodium > 10000)
nrow(HighSodium)

match("CAVIAR", USDA$Description)
USDA$Sodium[4154]

USDA$Sodium[match("CAVIAR", USDA$Description)]

summary(USDA$Sodium)
sd(USDA$Sodium)
sd(USDA$Sodium, na.rm=TRUE)

plot(USDA$Protein, USDA$TotalFat)
plot(USDA$Protein, USDA$TotalFat, xlab="Protein", ylab="Fat", main="Proteing vs Fat", col="red")

hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main="Histogram of Vitamin C Levels", xlim=c(0,100))
hist(USDA$VitaminC, xlab = "Vitamin C (mg)", main="Histogram of Vitamin C Levels", xlim=c(0,100), breaks = 2000)
boxplot(USDA$Sugar, main="Boxplot of Sugar Levels", ylab="Sugar")

HighSodium = USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE)
HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
USDA$HighSodium = as.numeric(USDA$Sodium > mean(USDA$Sodium, na.rm=TRUE))
USDA$HighFat = as.numeric(USDA$TotalFat > mean(USDA$TotalFat, na.rm=TRUE))
table(USDA$HighSodium, USDA$HighFat)



