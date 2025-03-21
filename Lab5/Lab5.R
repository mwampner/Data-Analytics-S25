library("ggplot2")
library(ggfortify)
library("readr")
library("e1071")
library("class")
library(caret)

setwd("C:/Users/wampnm/OneDrive/Data Analytics/Lab5")
wine <- read.csv("wine.csv") 
wine <- wine[-which(wine$Class==3),]

## column names
names(wine)

## split train/test
train.indexes <- sample(nrow(wine),0.75*nrow(wine))

train <- wine[train.indexes,]
test <- wine[-train.indexes,]

x <- wine[,2:13] 
y <- as.factor(as.matrix(wine[,1]))

attach(wine)

## Linear SVM
svr.mod0 <- svm(as.factor(Class) ~ (Total.phenols + Color.intensity + Ash + Magnesium + Flavanoids + Alcohol + Proline), train, kernel ="linear")

summary(svr.mod0)

svr.pred <- predict(svr.mod0, test)

##Accuracy
cm = as.matrix(table(Actual = test$Class, Predicted = svr.pred))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

#plot(svr.mod0, data = train, formula = train$Class ~ (train$Total.phenols + train$Color.intensity + train$Ash + train$Magnesium + train$Flavanoids + train$Alcohol + train$Proline), svSymbol = "x", dataSymbol = "o")
#svr.pred <- cbind(test$Class,svr.pred)
#ggplot(svr.pred, aes(x = V1, y = svr.pred)) +
#  geom_point() +
#  stat_smooth(method = "lm")

## Radial SVM
svr.mod0 <- svm(as.factor(Class) ~ (Total.phenols + Color.intensity + Ash + Magnesium + Flavanoids + Alcohol + Proline), train, kernel="radial")

summary(svr.mod0)

svr.pred <- predict(svr.mod0, test)

## Accuracy
cm = as.matrix(table(Actual = test$Class, Predicted = svr.pred))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

svr.outs <- data.frame(real=test$Class, pred=svr.pred)

## KNN Classification


# simple estimate of k
k = round(sqrt(nrow(wine)))

k <- k-1

knn.predicted <- knn(train = train, test = test, cl = train$Class, k = 11)

##Accuracy
#cm = as.matrix(table(Actual = train$Class, Predicted = knn.predicted))

cm <- table(knn.predicted, test$Class, dnn=list('predicted','actual'))
cm
# calculate classification accuracy
sum(diag(cm))/length(test$Class)

n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 
 
data.frame(precision, recall, f1)

## The SVM classifications both perform better than KNN, and the radial kernel 
## has more support vectors than the linear kernel 
               
setwd("C:/Users/wampnm/OneDrive/Data Analytics/Lab5")
house <- read.csv("NY-House-Dataset.csv") 
View(house)

train.indexes <- sample(nrow(house),0.75*nrow(house))

train <- house[train.indexes,]
test <- house[-train.indexes,]

svr.mod0 <- svm(PRICE ~ PROPERTYSQFT, train, kernel="linear")

summary(svr.mod0)

svr.pred <- predict(svr.mod0, test)

##Accuracy
cm = as.matrix(table(Actual = test$PRICE, Predicted = svr.pred))
cm
n = sum(cm) # number of instances
nc = nrow(cm) # number of classes
diag = diag(cm) # number of correctly classified instances per class 
rowsums = apply(cm, 1, sum) # number of instances per class
colsums = apply(cm, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted 

recall = diag / rowsums 
precision = diag / colsums
f1 = 2 * precision * recall / (precision + recall) 

data.frame(precision, recall, f1)

summary(svr.pred)

svr.pred <- cbind(test$PRICE,svr.pred)

ggplot(svr.pred, aes(x = V1, y = svr.pred)) +
  geom_point() +
  stat_smooth(method = "lm")

## Linear Model
lmod <- lm(house$PRICE ~ house$PROPERTYSQFT, data = house)
## print model output
summary(lmod)
