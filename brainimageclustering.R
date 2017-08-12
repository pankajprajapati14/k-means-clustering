healthy<- read.csv(file.choose(), header=F)
setwd("D:/analytics edge/clustering")
healthyMatrix <- as.matrix(healthy)
str(healthyMatrix)
healthyVector<- as.vector(healthyMatrix)
image(healthyMatrix, axes=F, col=grey(seq(0,1,length=256)))
distance= dist(healthyVector, method="euclidean")
str(healthyVector)
n= 365636
n*(n-1)/2
k=5
set.seed(1)
KMC= kmeans(healthyVector, centers=k, iter.max=1000)
healthyClusters= KMC$cluster
str(KMC)
KMC$centers[1]
dim(healthyClusters)<- c(nrow(healthyMatrix), ncol(healthyMatrix))
image(healthyClusters, axes=F, col=rainbow(k))
tumor<- read.csv(file.choose(), header=F)
tumorMatrix<- as.matrix(tumor)
tumorVector<- as.vector(tumorMatrix)
library(flexclust)
KMC.kcca<- as.kcca(KMC, healthyVector)
tumorClusters<- predict(KMC.kcca, newdata=tumorVector)
dim(tumorClusters)<- c(nrow(tumorMatrix), ncol(tumorMatrix))
image(tumorClusters, axes=F, col=rainbow(k))
image(tumorClusters, axes=F, col=grey(seq(0,1,length=256)))
