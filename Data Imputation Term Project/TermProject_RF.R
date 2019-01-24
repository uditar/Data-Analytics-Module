getwd()
install.packages("ggplot2")
install.packages("VIM")
install.packages("dplyr", dependencies=TRUE)
install.packages("gridExtra")
install.packages("reshape")
library("VIM")
library("dplyr")
library("ggplot2")
library("reshape")
library("tidyr")


Data <- read.csv("ProjectData.csv", header=TRUE, sep=",")
Data <- Data[,-1]
Data$Response <- factor(Data$Response, levels=c(0,1), labels=c("Yes", "No"))

Response <- as.factor(Data$Response)
temp2 <- within(Data, rm(Response))
Data1 <- cbind(temp2,Response)
class(Data1$Response)

n <- nrow (Data)


###################################################################################################################

##### Mean Imputation of multiple columns (i.e. the whole data frame) #####
data <- read.csv("ProjectData.csv", header=TRUE, sep=",")
data <- data[,-1]
data$Response <- factor(data$Response, levels=c(0,1), labels=c("Yes", "No"))
data$Group <- factor(data$Group, levels=c(0,1), labels=c("Female", "Female"))
for(i in 1:ncol(data)) {
  data[ , i][is.na(data[ , i])] <- mean(data[ , i], na.rm = TRUE)
}
#View(data)
set.seed(1234)
ind <- sample(2, nrow(data), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data[ind==1,]
validationData <- data[ind==2,]

#model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
tree = randomForest(Response ~ X1+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7+Group, ntree=500, proximity=T, data=trainData)


# Create the forest.
output.forest <- randomForest(Response ~ X1+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=trainData)

# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(fit,type = 2))


evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$Response)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$Response)/length(data$Response)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=5), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(tree, validationData, "class")

###################################################################################################################

##kNN imputation method
data <- read.csv("ProjectData.csv", header=TRUE, sep=",")
data <- data[,-1]
data$Response <- factor(data$Response, levels=c(0,1), labels=c("Yes", "No"))
data$Group <- factor(data$Group, levels=c(0,1), labels=c("Female", "Female"))
data_imputed <- kNN(data,k=3)

set.seed(1234)
ind <- sample(2, nrow(data_imputed), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data_imputed[ind==1,]
validationData <- data_imputed[ind==2,]

#model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
tree = randomForest(Response ~ X1+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7+Group, ntree=500, proximity=T, data=trainData)


# Create the forest.
output.forest <- randomForest(Response ~ X1+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=trainData)

# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(fit,type = 2))


evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$Response)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$Response)/length(data$Response)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=5), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(tree, validationData, "class")
##################################################################################################################
##Hot-deck imputation

data <- read.csv("ProjectData.csv", header=TRUE, sep=",")
data <- data[,-1]
data$Response <- factor(data$Response, levels=c(0,1), labels=c("Yes", "No"))
data$Group <- factor(data$Group, levels=c(0,1), labels=c("Female", "Female"))
data_imputed <- hotdeck(data)

set.seed(1234)
ind <- sample(2, nrow(data_imputed), replace=TRUE, prob=c(0.7, 0.3))
trainData <- data_imputed[ind==1,]
validationData <- data_imputed[ind==2,]

#model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
tree = randomForest(Response ~ X1+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7+Group, ntree=500, proximity=T, data=trainData)


# Create the forest.
output.forest <- randomForest(Response ~ X1+X4+X5+X6+X7+Y1+Y2+Y3+Y4+Y5+Y6+Y7, data=trainData)

# View the forest results.
print(output.forest) 

# Importance of each predictor.
print(importance(fit,type = 2))


evaluation <- function(model, data, atype) {
  cat("\nConfusion matrix:\n")
  prediction = predict(model, data, type=atype)
  xtab = table(prediction, data$Response)
  print(xtab)
  cat("\nEvaluation:\n\n")
  accuracy = sum(prediction == data$Response)/length(data$Response)
  precision = xtab[1,1]/sum(xtab[,1])
  recall = xtab[1,1]/sum(xtab[1,])
  f = 2 * (precision * recall) / (precision + recall)
  cat(paste("Accuracy:\t", format(accuracy, digits=5), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(tree, validationData, "class")
###################################################################################################################

