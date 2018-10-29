install.packages("klaR")
install.packages('cba')

library(klaR)
library(cba)
Mushrooms= read.csv("Mushroom.csv")
str(Mushrooms)

Index= apply(Mushrooms, 2, function(variable) any(is.na(variable) | is.infinite(variable)))
colnames(Mushrooms)[Index]

table(Mushrooms$Class)

for (z in 1:7) {
  plot(Mushrooms[ ,z], main=colnames(Mushrooms)[z],
       ylab = "Count", col="green", las = 0)
}


Mushrooms.torun = subset(Mushrooms, select = -c(Class, Veil.type))

##One-hot encoded data
##This is basically creating dummy variables for each value of the category, for all the variables
Mushrooms.torun.ohe = model.matrix(~.-1, data=Mushrooms.torun)
str(Mushrooms.torun.ohe)

# Using the elbow method to find the optimal number of clusters
w.stand <- scale(Mushrooms.torun.ohe[-1])
X=Mushrooms.torun.ohe[-1]
wssplot <- function(X, nc=15, seed=1234){
  wss <- (nrow(X)-1)*sum(apply(X,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(X, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}



set.seed(20) #for reproducibility
#nstart = 50, indicates R will run 50 different random starting assignments and selects the lowest within cluster variation

#Applying kmeans clustering----------------------->


result.kmean = kmeans(Mushrooms.torun.ohe, 2, nstart = 50, iter.max = 15) 
#Purity of clustering is a simple measure of the accuracy, which is between 0 and 1. 0 indicates poor clustering, and 1 indicates perfect clustering.

result.kmean.mm <- table(Mushrooms$Class, result.kmean$cluster)
purity.kmean <- sum(apply(result.kmean.mm, 2, max)) / nrow(Mushrooms.torun)
purity.kmean

result.kmean$cluster
result.kmean$withinss
result.kmean$centers

#Applying kmode clustering----------------------------->


kmode_clus <- kmodes(Mushrooms.torun, 2, iter.max = 50, weighted = FALSE)
kmode_clus.mm <- table(Mushrooms$Class, kmode_clus$cluster)
kmode_Purity_efficient <- sum(apply(kmode_clus.mm, 2, max)) / nrow(Mushrooms.torun)

kmode_clus$cluster
kmode_clus$withinss
kmode_clus$centers