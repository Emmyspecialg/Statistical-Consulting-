library(tidyverse)
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(dbplyr)
library(tidyr)
library(rmarkdown)

#importing csv file
exampleg <- read_delim("ApplStat-ExampleG.dat")
examplegnew <- exampleg
examplegnew[is.na(examplegnew) | examplegnew == "Inf"] = NA

#1a- exploring the data, take log S,T1, T2, N 
pairs(exampleg)
model <- lm(log(C) ~ D + log(T1) + log(T2) + log(S) + PR + NE + CT + BW + log(N) + PT, data = examplegnew)
summary(model)

#-0.22429x + 0.12246 shows the PT 
exp(-0.22429)
#0.7990834 -- 20% decrease if you have one??  

#1b- final reduced model, removed t1,t2,pr,bw
model2 <- lm(log(C) ~ PT + CT + log(N) + log(S) + D + NE, data = examplegnew)
summary(model2)

#1c- resid plot of fitted values (g3)-- checks if it is linear vs nonlinear 
res <- resid(model2)
plot(fitted(model2),res, main= "Figure 1: Residuals vs Fitted Values", xlab= "Fitted Values", ylab="Residuals") + abline(0,0,lty=2)

#Residuals vs log N, want to see its effect with the model, if curve, its not just variable 
logN <- log(examplegnew$N)
plot(logN,res, main="Figure 2: Residuals vs LogN", xlab= "Log N", ylab="Residuals") + abline(0,0,lty=2)

#Residuals vs normal order statistics (g4) -- checks for normality , ask if linear dotted line is okay 
qqnorm(res, main="Figure 3: Residuals vs Normal Order Statistics", xlab= "Normal order statistics", ylab="Residuals") + qqline(res, lty=2)

#1d use this website: https://online.stat.psu.edu/stat501/lesson/4/4.2
#normal check for normality, since it is assumed its normal -- normal order stats 

#1e creating a multiple linear regression interaction with one variable 
model3 <- lm(log(C) ~ PT + CT + log(N) + log(S) + D + NE + (PT*log(S)), data = examplegnew)
summary(model3)

#2 importing data 
cancer <- read_delim("Cancer.txt")
cancer
cor(cancer)
pairs(cancer)
cmodel1 <- lm(lpsa ~ lcavol + lweight + age + lbph + svi + lcp + gleason + pgg45, data=cancer)
coef(cmodel1)
summary(cmodel1)
c.res1 <- resid(cmodel1)
plot(fitted(cmodel1),c.res1, main= "Residuals vs Fitted Values", xlab="Fitted Values", ylab="Residuals") + abline(0,0,lty=2)
qqnorm(c.res1) + qqline(c.res, lty=2, col="red")


#we would want to remove lweight, lcavol, and svi since its mad high in estimate 
cmodel2 <- lm(lpsa ~ lweight + lcavol + svi + lbph + age  , data=cancer)
summary(cmodel2)


#correlation matrix
round(cor(cancer),4)

#residual plot w fitted values , qqplot, and then pick a variable 
c.res <- resid(cmodel2)
plot(fitted(cmodel2),c.res, main= "Figure 5: Residuals vs Fitted Values", xlab="Fitted Values", ylab="Residuals") + abline(0,0,lty=2)
qqnorm(c.res, main="Figure 6: Residuals vs Normal Order Statistics") + qqline(c.res, lty=2, col="red")

plot(cancer$age,c.res) + abline(0,0,lty=2)
plot(cancer$lbph,c.res) + abline(0,0,lty=2)
plot(cancer$lcavol,c.res) + abline(0,0,lty=2) 
plot(cancer$lweight,c.res) + abline(0,0,lty=2)
plot(cancer$svi,c.res) + abline(0,0,lty=2)

#3 Log Likelihood

#likelihood
x <- seq(0,1,length=100)
L <- function(q){q^6* (1-q)^4}
plot(x,L(x),ylab="L(x)",xlab="x",type="l") + abline(v=0.6, lty=2)

# p= 0.6 -- find CI of 0.6?


#log likelihood
p <- seq(0,1,length=100)
l<- function(p){6*log(p) + 4* log(1-p)}
which.max(l(p))
plot(x,L(x),ylab="L(x)",xlab="x",type="l") + abline(v=0.6, lty=2)
plot(p,l(p)-l(0.6),ylab="l(p) - l(phat)",xlab="p", main="Figure 7: Log Likelihood", type="l",ylim=c(-10,5)) + abline(v=0.92, h= c(0,-1.92), lty=2) + lines(x,t(x)-t(0.6),ylab="l(x) - l(phat)", xlab="x",type="l",col="red",ylim=c(-10,5)) + lines(o,j(o)-j(0.6),ylab="l(x) - l(phat)", col="blue", xlab="x",type="l",ylim=c(-10,5))
legend(x="topleft", title="# of Positives", cex = 0.7,legend = c("18/30 ", "6/10", "3/5"), lty = c(1, 1, 1), col = c(2, 1, 4))
l(p)
which.max(p)

plot(p,l(p)-l(0.6),ylab="l(p) - l(phat)",xlab="p", main="Figure 7: Log Likelihood", type="l",ylim=c(-10,5)) 

#18 positives out of 30
x <- seq(0,1,length=100)
t <- function(p){18*log(p) + 12* log(1-p)}
plot(x,t(x)-t(0.6),ylab="l(x) - l(phat)", main= "Figure 8: Log Likelihood: 18/30 Positives", xlab="x",type="l",ylim=c(-10,5)) + abline(v=0.6, lty=2) #log likeli

# 3 positives out of 5
k <- table(p,l(p)-l(0.6))
o <- seq(0,1,length=100)
j <- function(p){3*log(p) + 2* log(1-p)}
plot(o,j(o)-j(0.6),ylab="l(x) - l(phat)", main= "Figure 9: Log Likelihood: 3/5 Positives", xlab="x",type="l",ylim=c(-10,5)) + abline(v=0.6, lty=2)

# log likelihood have the same max but the shapes are either wider or narrower, the larger the numbers the narrower the curve is. 














