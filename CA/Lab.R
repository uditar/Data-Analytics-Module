X <- c(12.8,10.5,13.2,13.0,7.0,11.0,13.4,13.2,9.5,11.0,10.9,4.6,5.8,3.2,9.8,0.2,11.2,7.2,14.7,5.9,9.7,17.6,8.5,6.8,7.2,12.2,16.7,10.4,14.2,5.7)
View(X)
variance <- var(X)
View(variance)
View(length(X))

tow <- variance #initial tow
init_tow <- 0.25
Mu <- 8
sigma <- 4
Beta <- 1
n <- length(X)
alpha <- 5
MeanofNormal <- 0
TowMean<- 0
do_once <- function (){
 
SampleMean <- (n*mean(X)*tow + Mu*ini_tow)/(n*tow+ini_tow)
SampleDeviation <- 1/(n*tow + ini_tow)

Normal <- rnorm(1000,SampleMean,SampleDeviation)
MeanofNormal <- mean(Normal)
View(MeanofNormal)
Mu <- MeanofNormal

S <- sum((X-MeanofNormal))^2
View(S)
MeanGamma <- Beta + (S/2)
varianceGamma <- alpha + (n/2)

Gamma <- rgamma(1000,shape=varianceGamma,rate=MeanGamma)
View(Gamma)
TowMean <- mean(Gamma)
View(TowMean)

tow <- (TowMean)

}
replicate(500,do_once())


