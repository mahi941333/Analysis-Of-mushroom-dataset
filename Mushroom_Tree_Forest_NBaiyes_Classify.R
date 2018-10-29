#Random Forest Classification
install.packages("tidyverse")
library(tidyverse)
# Importing the dataset
Mushrooms = read.csv('Mushroom.csv')

#checking for missing values
map_dbl(Mushrooms, function(.variable) {sum(is.na(.variable))})
#No missing values


# Encoding the target feature as factor
## We make each variable as a factor
Mushrooms <- Mushrooms %>% map_df(function(.variable) as.factor(.variable))

## We redefine each of the category for each of the variables
levels(Mushrooms$Class) <- c("edible", "poisonous")
levels(Mushrooms$Cap.shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(Mushrooms$Cap.color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                 "green", "purple", "white", "yellow")
levels(Mushrooms$Cap.surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(Mushrooms$Bruises) <- c("no", "yes")
levels(Mushrooms$Odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(Mushrooms$Gill.attachment) <- c("attached", "free")
levels(Mushrooms$Gill.spacing) <- c("close", "crowded")
levels(Mushrooms$Gill.size) <- c("broad", "narrow")
levels(Mushrooms$Gill.color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                  "pink", "green", "purple", "white", "yellow")
levels(Mushrooms$Stalk.shape) <- c("enlarging", "tapering")
levels(Mushrooms$Stalk.root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(Mushrooms$Stalk.surface.above.ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(Mushrooms$Stalk.surface.below.ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(Mushrooms$Stalk.color.above.ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                              "green", "purple", "white", "yellow")
levels(Mushrooms$Stalk.color.below.ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                              "green", "purple", "white", "yellow")
levels(Mushrooms$Veil.type) <- "partial"
levels(Mushrooms$Veil.color) <- c("brown", "orange", "white", "yellow")
levels(Mushrooms$Ring.number) <- c("none", "one", "two")
levels(Mushrooms$Ring.type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(Mushrooms$Spore.print.color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                         "green", "purple", "white", "yellow")
levels(Mushrooms$Population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(Mushrooms$Habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

summary(Mushrooms)

#taking away veil_type variables

Mushrooms.torun = subset(Mushrooms, select = -c(Class, Veil.type))


#visualise the data

library(ggplot2)
ggplot(Mushrooms, aes(x = Cap.surface, y = Cap.color, col = Class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("Blue", "brown"))

ggplot(Mushrooms, aes(x = Class, y = Odor, col = Class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("violet", "orange"))

ggplot(Mushrooms, aes(x = Cap.shape, y = Cap.color, col = Class)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("light blue", "brown"))

# Splitting the dataset into the Training set and Test set
install.packages('caTools')
library(caTools)
set.seed(263)
split = sample.split(Mushrooms$Class, SplitRatio = 0.80)
Training_Mdata = subset(Mushrooms, split == TRUE)
test_Mdata = subset(Mushrooms, split == FALSE)




#######################


# Fitting Decision Tree Classification to the Training set---------------------------->
install.packages('rpart')
library(rpart)
classifier = rpart(formula = Class ~ .,
                   data = Training_Mdata , cp=0.000001)

# Predicting the Test set results
Class_Pred1 = predict(classifier, newdata = test_Mdata, type = 'class')


# Making the Confusion Matrix
install.packages('caret')
install.packages('e1071')

caret::confusionMatrix(data=predict(classifier, type = "class"), 
                       reference = Training_Mdata$Class, 
                       positive="edible")



plotcp(classifier)
Effective_cp<- round(classifier$cptable[which.min(classifier$cptable[, "xerror"]), "CP"], 4)
Classifier_Pruned <- prune(classifier, cp = Effective_cp)


# Visualising the decision tree

install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(classifier,extra =  3,fallen.leaves = T)




# Plotting the tree
plot(classifier)
text(classifier)



# Fitting Random Forest Classification to the Training set-------------------------------->
install.packages('randomForest')
library(randomForest)
set.seed(123)
Forest_classifier = randomForest(x = Training_Mdata,
                                 y =Training_Mdata$Class,
                                 ntree = 500)

# Predicting the Test set results
Class_Pred2 = predict(Forest_classifier, newdata = test_Mdata, type = 'class')


# Making the Confusion Matrix
install.packages('caret')
install.packages('e1071')

caret::confusionMatrix(data=predict(Forest_classifier, type = "class"), 
                       reference = Training_Mdata$Class, 
                       positive="edible")






# Visualising the Random Forest
varImpPlot(Forest_classifier, sort = TRUE, 
           n.var = 10, main = "The 10 variables with the most predictive power")




# Plotting the Forest
plot(Forest_classifier)



# Fitting Naive Bayes Classification to the Training set-------------------------->

#install.packages('e1071')
library(e1071)

classifier3 = naiveBayes(x = Training_Mdata,
                        y = Training_Mdata$Class)
# Predicting the Test set results
Class_Pred3 = predict(classifier3, newdata = test_Mdata,type='class')


library(caret)

table(Class_Pred3, test_Mdata$Class)


