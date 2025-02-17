###################
##### Abalone #####
###################

library(class)
library(caret)

# read dataset
setwd("C:/Users/wampnm/OneDrive/Data Analytics/Lab3")
abalone <- read.csv("abalone_dataset.csv")
View(abalone)

dataset <- abalone

## add new column age.group with 3 values based on the number of rings 
dataset$age.group <- cut(dataset$rings, br=c(0,8,11,35), labels = c("young", 'adult', 'old'))
dataset <- na.omit(dataset)
View(dataset)

## alternative way of setting age.group
#dataset$age.group[dataset$rings<=8] <- "young"
#dataset$age.group[dataset$rings>8 & dataset$rings<=11] <- "adult"
#dataset$age.group[dataset$rings>11 & dataset$rings<=35] <- "old"

## Exercise 1

# 2924 <- 70%
n = 2924
s_abalone <- sample(n,n*.8)

## create train & test sets based on sampled indexes 
dataset.train <- dataset[s_abalone,]

dataset.test <- dataset[-s_abalone,]

# simple estimate of k
k = round(sqrt(n))

k <- k-1

# subset 1: length, height, whole_weight
subset1 <- c("length", "height", "diameter")

knn.predicted1 <- knn(train = dataset.train[,subset1], test = dataset.test[,subset1], cl = dataset.train$age.group, k = 11)
# create contingency table/ confusion matrix 
contingency.table1 <- table(knn.predicted, dataset.test$age.group, dnn=list('predicted','actual'))
contingency.table1
# calculate classification accuracy
sum(diag(contingency.table1))/length(dataset.test$age.group)

# subset 2: diameter, shucked_weight, viscera_weight
subset2 <- c("whole_weight", "shucked_wieght", "viscera_wieght")
knn.predicted2 <- knn(train = dataset.train[,subset2], test = dataset.test[,subset2], cl = dataset.train$age.group, k = 11)
# create contingency table/ confusion matrix 
contingency.table2 <- table(knn.predicted2, dataset.test$age.group, dnn=list('predicted','actual'))
contingency.table2
# calculate classification accuracy
sum(diag(contingency.table2))/length(dataset.test$age.group)

# Subset 2 has higher accuracy
# optimal value k

## train and evaluate multiple knn models to find optimal k
knn.model <- train(dataset[,subset2], dataset$age.group, method = "knn", tuneLength = 20, trControl = trainControl(method = "cv"))

# print model outputs
print(knn.model)

# Optimal k value is 35 (.6812826 = accuracy)

# Exercise 2 (best performing -> subset2)
# "whole_weight", "shucked_wieght", "viscera_wieght"
# using best k?
k = 35
abalone.km <- kmeans(dataset[,subset2], centers = k)

assigned.clusters <- as.factor(abalone.km$cluster)

ggplot(dataset, aes(x = viscera_wieght, y = shucked_wieght, colour = as.factor(assigned.clusters))) +
  geom_point()

k.list <- c(7,9,11,19,25,29,31,37,39,41)

wcss.list <- c()

for (k in k.list) {
  
  abalone.km <- kmeans(dataset[,subset2], centers = k)
  
  wcss <- abalone.km$tot.withinss
  
  wcss.list <- c(wcss.list,wcss)
  
  ## get and plot clustering output 
  assigned.clusters <- as.factor(abalone.km$cluster)
  
  ggplot(dataset, aes(x = shucked_wieght, y = viscera_wieght, colour = assigned.clusters)) +
    geom_point()
  
}

plot(k.list,wcss.list,type = "b")

# based on wcss plot, it looks flattest at 30
