setwd("C:\\Users\\Romanos\\Projects\\rworkspace\\TheAnalyticsEdge\\Unit6 - Clustering")

# Problem 1.1 - Hierarchical Clustering
dailykos = read.csv("data\\dailykos.csv")
summary(dailykos)
str(dailykos)
dailykosVector = as.vector(dailykos)
str(dailykosVector)
distance = dist(dailykos, method = "euclidean")
clusterIntensity = hclust(distance, method="ward.D")

# Problem 1.2 - Hierarchical Clustering
plot(clusterIntensity)

# Problem 1.4 - Hierarchical Clustering
clusterGroups = cutree(clusterIntensity, k = 7)
cluster1 = subset(dailykos, clusterGroups==1)
cluster2 = subset(dailykos, clusterGroups==2)
cluster3 = subset(dailykos, clusterGroups==3)
cluster4 = subset(dailykos, clusterGroups==4)
cluster5 = subset(dailykos, clusterGroups==5)
cluster6 = subset(dailykos, clusterGroups==6)
cluster7 = subset(dailykos, clusterGroups==7)
nrow(cluster7)

# Problem 1.5 - Hierarchical Clustering
tail(sort(colMeans(cluster1)))

# Problem 1.6 - Hierarchical Clustering
tail(sort(colMeans(cluster2)))
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))

# Problem 2.1 - K-Means Clustering
set.seed(1000)
KMC = kmeans(dailykos, centers = 7)
str(KMC)
clusters = KMC$cluster
str(clusters)
KMCcluster1 = subset(dailykos, clusters==1)
KMCcluster2 = subset(dailykos, clusters==2)
KMCcluster3 = subset(dailykos, clusters==3)
KMCcluster4 = subset(dailykos, clusters==4)
KMCcluster5 = subset(dailykos, clusters==5)
KMCcluster6 = subset(dailykos, clusters==6)
KMCcluster7 = subset(dailykos, clusters==7)
nrow(KMCcluster3)

# Problem 2.2 - K-Means Clustering
tail(sort(colMeans(KMCcluster1)))
tail(sort(colMeans(KMCcluster2)))
tail(sort(colMeans(KMCcluster3)))
tail(sort(colMeans(KMCcluster4)))
tail(sort(colMeans(KMCcluster5)))
tail(sort(colMeans(KMCcluster6)))
tail(sort(colMeans(KMCcluster7)))

# Problem 2.3 - K-Means Clustering
table(clusterGroups, clusters)

# Problem 2.4 - K-Means Clustering
table(clusterGroups, clusters)

# Problem 2.5 - K-Means Clustering
table(clusterGroups, clusters)

# Problem 2.6 - K-Means Clustering
table(clusterGroups, clusters)