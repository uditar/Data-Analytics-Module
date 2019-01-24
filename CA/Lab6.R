getwd()
install.packages("ggplot2")
library(ggplot2)
Data <- read.csv("siswave3v4impute3.csv", header=TRUE, sep=",")
View(Data)
attach(Data)
n <- nrow (Data)


# earnings variables:
# rearn:  respondent's earnings
# tearn:  spouse's earnings
# set up some simplified variables to work with
na.fix <- function (a) {
  ifelse (a<0 | a==999999, NA, a)
}
earnings <- na.fix(rearn) + na.fix(tearn)
earnings <- earnings/1000
##############################################################################################################
#Missing data in R and bugs from Pg 529
cbind (Data$sex, Data$race, Data$educ_r, Data$r_age, earnings, Data$police)[91:95,]
##############################################################################################################
#random imputation of single variable - earnings Pg 534
random.imp <- function (a){
  missing <- is.na(a)
  n.missing <- sum(missing)
  a.obs <- a[!missing]
  imputed <- a
  imputed[missing] <- sample (a.obs, n.missing, replace=TRUE)
  return (imputed)
}

earnings.imp <- random.imp (earnings)

#Zero coding or topcoding 
topcode <- function (a, top){
  return (ifelse (a>top, top, a))
}
earnings.top <- topcode (earnings, 100) # earnings are in $thousands topcoded to 100
#Pg 534 fig. 25.1 a
hist (earnings.top[earnings>0], xlab = "earnings",ylab="", main = "Observed earnings (excluding 0's)") 


###############################################################################################################
#Deterministic imputation of single variable - earnings Pg 535

#calculate each variable
white <- ifelse (race==1, 1, 0)
white[is.na(race)] <- 0
male <- ifelse (sex==1, 1, 0)
over65 <- ifelse (r_age>65, 1, 0)
immig[is.na(immig)] <- 0
educ_r[is.na(educ_r)] <- 2.5
workhrs.top <- topcode (workhrs, 40)
is.any <- function (a) {
  any.a <- ifelse (a>0, 1, 0)
  any.a[is.na(a)] <- 0
  return(any.a)
}

workmos <- workmos
earnings[workmos==0] <- 0
any.ssi <- is.any (ssi)
any.welfare <- is.any (welfare)
any.charity <- is.any (charity)

#setting up a data frame with all the variables we shall use in our analysis:
sis <- data.frame (cbind (earnings, earnings.top, male, over65, white, 
                          immig, educ_r, workmos, workhrs.top, any.ssi, any.welfare, any.charity))

#fit a regression to positive values of earnings
lm.imp.1 <- lm (earnings ~ male + over65 + white + immig + educ_r +
                workmos + workhrs.top + any.ssi + any.welfare + any.charity,
                data=sis, subset=earnings>0)
#predictions for the data
pred.1 <- predict (lm.imp.1, sis)

#imputing predictions into missing values
impute <- function (a, a.impute){ 
  ifelse (is.na(a), a.impute, a)
}
#compute missing earnings
earnings.imp.1 <- impute (earnings, pred.1)
View(earnings.imp.1)
#transforming and top coding 
lm.imp.2.sqrt <- lm (I(sqrt(earnings.top)) ~ male + over65 + white + 
                     immig + educ_r + workmos + workhrs.top + any.ssi + any.welfare +
                       any.charity, data=sis, subset=earnings>0)


pred.2.sqrt <- predict (lm.imp.2.sqrt, sis)
pred.2 <- topcode (pred.2.sqrt^2, 100)
earnings.imp.2 <- impute (earnings.top, pred.2)


#as tabulated on Pg 536
summary(lm.imp.2.sqrt)
View(sd(earnings.imp.2))

#Plot deterministic imputation of earnings, Pg 534 fig. 25.1b
hist (earnings.imp.2[is.na(earnings)], xlab = "earnings", ylab="", main = "Deterministic imputation of earnings") 

#plot using ggplot2 just for practice
frame2 = data.frame(earnings = earnings.imp.2[is.na(earnings)])
p2 <- ggplot(frame2,aes(earnings)) +
  geom_histogram(colour = "black", fill = "white",binwidth=7) +
  theme_bw() +
  labs(title="Deterministic imputation of earnings")
plot(p2)

###############################################################################################################

?rnorm
## Random regression imputation
pred.4.sqrt <- rnorm (n, pred.2.sqrt, sigma(lm.imp.2.sqrt))
pred.4 <- topcode (pred.4.sqrt^2, 100)
earnings.imp.4 <- impute (earnings.top, pred.4)
View(mean(earnings.imp.4))
View(sd(earnings.imp.4))
#Plot random imputation of earnings, Pg 534 fig. 25.1c
hist (earnings.imp.4[is.na(earnings)], xlab = "earnings", ylab="", main = "Random imputation of earnings")

###############################################################################################################

#Two-stage modeling to impute a variable that can be positive or zero, Pg 538
#Applying generalized linear model 
glmfit <- glm (I(earnings>0) ~ male + over65 + white +
                   immig + educ_r + any.ssi + any.welfare + any.charity,
                 data=sis, family=binomial(link=logit))
summary(glmfit)
lm.ifpos.sqrt <- lm (I(sqrt(earnings.top)) ~ male + over65 + white +
                       immig + educ_r + any.ssi + any.welfare + any.charity,
                     data=sis, subset=earnings>0) # (same as lm.imp.2 from above)
View(lm.ifpos.sqrt)

#?rbinom
#impute whether missing earnings are positive
pred.sign <- rbinom (n, 1, predict(glmfit,sis, type = "response"))
pred.pos.sqrt <- rnorm (n, predict (lm.ifpos.sqrt, sis),
                        sigma(lm.ifpos.sqrt))
#then impute the earnings themselves
pred.pos <- topcode (pred.pos.sqrt^2, 100)
earnings.imp <- impute (earnings, pred.sign*pred.pos)
View(mean(earnings.imp))
View(sd(earnings.imp))
hist(earnings.imp[is.na(earnings)], xlab = "earnings", main = "Two stage modeling of earnings")
