setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit6 - Clustering")

# Problem 1.1 - Normalizing the Data
airlines = read.csv("data\\AirlinesCluster.csv")
summary(airlines)

# Problem 1.3 - Normalizing the Data
library(caret)
airlinesNorm
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
summary(airlinesNorm)
sd(airlinesNorm)

# Problem 2.1 - Hierarchical Clustering
distance = dist(airlinesNorm, method = "euclidean")
clusterIntensity = hclust(distance, method="ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity, k = 7, border = "red")
rect.hclust(clusterIntensity, k = 6, border = "green")

# Problem 2.2 - Hierarchical Clustering
clusterGroups = cutree(clusterIntensity, k = 5)
cluster1 = subset(airlinesNorm, clusterGroups==1)
nrow(cluster1)

# Problem 2.3 - Hierarchical Clustering
tapply(airlines$Balance, clusterGroups, mean)
tapply(airlines$QualMiles, clusterGroups, mean)
tapply(airlines$BonusMiles, clusterGroups, mean)
tapply(airlines$BonusTrans, clusterGroups, mean)
tapply(airlines$FlightMiles, clusterGroups, mean)
tapply(airlines$FlightTrans, clusterGroups, mean)
tapply(airlines$DaysSinceEnroll, clusterGroups, mean)

# Problem 3.1 - K-Means Clustering
set.seed(88)
KMC = kmeans(airlinesNorm, centers = 5, iter.max = 1000)
clusters = KMC$cluster
KMCcluster1 = subset(airlinesNorm, clusters==1)
KMCcluster2 = subset(airlinesNorm, clusters==2)
KMCcluster3 = subset(airlinesNorm, clusters==3)
KMCcluster4 = subset(airlinesNorm, clusters==4)
KMCcluster5 = subset(airlinesNorm, clusters==5)
KMCcluster6 = subset(airlinesNorm, clusters==6)
KMCcluster7 = subset(airlinesNorm, clusters==7)
nrow(KMCcluster5)

# Problem 3.2 - K-Means Clustering
KMC$centers



