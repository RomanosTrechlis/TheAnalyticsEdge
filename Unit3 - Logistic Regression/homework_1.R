setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit3 - Logistic Regression")

songs = read.csv("data\\songs.csv")

# Understanding the data
str(songs)
str(subset(songs, year == 2010))
str(subset(songs, artistname == 'Michael Jackson'))
subset(songs, artistname == 'Michael Jackson' & Top10 == 1)$songtitle
table(songs$timesignature)
songs$songtitle[which.max(songs$tempo)]

# Creating the Prediction Model
SongsTrain = subset(songs, year <= 2009)
SongsTest = subset(songs, year == 2010)
## Exclude some variables from data frame
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
str(SongsTrain)
SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]
Model1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(Model1)

# Beware of Multicollinearity Issues!
cor(SongsTrain$loudness, SongsTrain$energy)
SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)
SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

# Validating Our Model
PredLog3 = predict(SongsLog3, newdata=SongsTest, type="response")
table(SongsTest$Top10, PredLog3 >= 0.45)
(309 + 19)/(309 + 5 + 40 + 19)
table(SongsTest$Top10)
314 / (314 + 59)
19 / (40 + 19)
309 / (309 + 5)
