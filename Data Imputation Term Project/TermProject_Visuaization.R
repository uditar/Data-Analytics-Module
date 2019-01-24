###############################################################################################################
##Visualizations##
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

par(mfrow=c(1,1))
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
