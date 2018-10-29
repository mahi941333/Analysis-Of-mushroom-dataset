install.packages("klaR")
install.packages('cba')

library(klaR)
library(cba)
Mushrooms= read.csv("Mushroom.csv")
str(Mushrooms)
X=Mushrooms[-1]
#Checking missing values
Index= apply(Mushrooms, 2, function(variable) any(is.na(variable) | is.infinite(variable)))
colnames(Mushrooms)[Index]

table(Mushrooms$Class)

#plot
for (z in 1:7) {
  plot(Mushrooms[ ,z], main=colnames(Mushrooms)[z],
       ylab = "Count", col="green", las = 0)
}

#removing viel_type variable which has single values
Mushrooms.torun = subset(Mushrooms, select = -c(Class, Veil.type))

##One-hot encoded data
##This is basically creating dummy variables for each value of the category, for all the variables
Mushrooms.torun.ohe = model.matrix(~.-1, data=Mushrooms.torun)
str(Mushrooms.torun.ohe)

##getting the X dataset
X=Mushrooms.torun.ohe[-1]
d =dist(Mushrooms.torun.ohe, method = "euclidean") # Euclidean distance matrix.

H.fit <- hclust(d, method="ward.D2")
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=3) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(H.fit, k=3, border="red") 

table(Mushrooms.torun.ohe[,1],groups)