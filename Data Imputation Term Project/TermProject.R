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


Response <- as.factor(Data$Response)
temp2 <- within(Data, rm(Response))
Data1 <- cbind(temp2,Response)
class(Data1$Response)

n <- nrow (Data)

###############################################################################################################
##Visualizations##

Data %>% gather() %>% head()

#distribution plot of variables
ggplot(gather(Data), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

X1 <- Data$X1
X2 <- Data$X2
X3 <- Data$X3
X4 <- Data$X4
X5 <- Data$X5
X6 <- Data$X6
X7 <- Data$X7
par(mfrow=c(2,2))
hist(X1)
boxplot(X1, horizontal=TRUE)
hist(X2)
boxplot(X2, horizontal=TRUE)
hist(X3)
boxplot(X3, horizontal=TRUE)
hist(X4)
boxplot(X4, horizontal=TRUE)
hist(X5)
boxplot(X5, horizontal=TRUE)
hist(X6)
boxplot(X6, horizontal=TRUE)
hist(X7)
boxplot(X7, horizontal=TRUE)

##################################################################################################################

#Use aggregate plots to visualize missing data
aggr(Data, col = c("green", "red"),sortVars = TRUE,
     sortCombs = TRUE, numbers = TRUE, prop = TRUE)

#################################################################################################################
#Use Correlations to explore missing values
cor(Data)
cor(Data, use="complete.obs")
symnum(cor(Data, use="pairwise"))

install.packages("corrplot")
library(corrplot)
M<-cor(Data, use="pairwise")
head(round(M,2))
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
?corrplot
corrplot(M, method="color", col=col(200),  
         type="upper", order="original", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         #p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)


##################################################################################################################
mtmelt <- melt(Data, id = "Response")
?ggplot
ggplot(mtmelt, aes(x = value, y = Response)) +
  facet_wrap(~variable, scales = "free") +
  geom_point()

Data %>%
  gather(-Response, -Group, key = "var", value = "value") %>% 
  ggplot(aes(x = value, y = Response, colour=Group, shape = factor(Group))) +
  geom_point() +
  facet_wrap(~ var, scales = "free") +
  theme_bw()
###################################################################################################################

#Baseline DT model without imputations and considerig all variables or features
install.packages("rpart")
install.packages("partykit")
library("rpart")
library("partykit")


DT_Model <- rpart(Data$Response~.,data=Data, control=rpart.control(minsplit=60,minbucket=30,maxdepth=5))
print(DT_Model)

pred <- predict(DT_Model, newdata=Data)
install.packages("caret")
library(caret)
confusionMatrix(pred, Data$Response)

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
  cat(paste("Accuracy:\t", format(accuracy, digits=2), "\n",sep=" "))
  cat(paste("Precision:\t", format(precision, digits=2), "\n",sep=" "))
  cat(paste("Recall:\t\t", format(recall, digits=2), "\n",sep=" "))
  cat(paste("F-measure:\t", format(f, digits=2), "\n",sep=" "))
}
evaluation(DT_Model, Data, "vector")

###################################################################################################################

##### Mean Imputation of multiple columns (i.e. the whole data frame) #####

for(i in 1:ncol(Data)) {
  Data[ , i][is.na(Data[ , i])] <- mean(Data[ , i], na.rm = TRUE)
}
View(Data)

###################################################################################################################
