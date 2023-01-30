library(tidyverse)
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(dbplyr)
library(tidyr)


#importing csv file
donations <- read.csv("Donations.csv", header=T)
head(donations, n=3)
donations

#dimensions
nrow(donations)
ncol(donations)
mean(donations$Amount, na.rm = T)
summary(donations$Amount)
min(donations$Amount, na.rm=T)
which.min(donations$Amount)

#amount vs week (need to add title and axis) time plot, box plot of amount 
library(ggplot2)
ggplot(donations, mapping= aes(Week, Amount)) + geom_line() + labs(title="Donations Over Time")
ggplot(donations, aes(Amount)) + geom_boxplot() 

boxplot(donations$Amount, main= "Donation Amount Spread", xlab="Donation Amount") #find how to add axis 

#group by month ?? dec 30,2011
topdates <- donations %>% select(Date, Amount) %>% arrange(desc(Amount))
head(topdates, n=3)

#importing waterdat 
waterdat <- read.csv("Water.dat", header = T)
waterdatnew <- waterdat %>% separate("Temp.Pressure", into= c("Temps", "Pressures"), sep = " ") %>% mutate(Temps = as.numeric(Temps), Pressures = as.numeric(Pressures))
waterdatnew


#training linear model, x is temp, y is pressure, look at anova and package for confidence interval
final <- lm(Pressures ~ Temps, data = waterdatnew)
anova(final)
summary(final)

#plotting data
qplot(waterdatnew$Temps, waterdatnew$Pressures)
ggplot(waterdatnew, aes(Temps,Pressures)) + geom_point() 
plot(waterdatnew$Temps, waterdatnew$Pressures, main= "Pressures vs. Temperature", xlab="Temperature (F)", ylab="Pressures (mm Hg)") + abline(lm(waterdatnew$Pressures ~ waterdatnew$Temps)) 

#residual plot, U shaped , tells us a nonlinear equation fits our model better
res <- resid(final)
plot(fitted(final), res, main="Figure 4: OLS Resid Plot") + abline(0,0) 

#boxcox procedure, suggests what model we should use 
library(MASS)
boxcoxplot <- boxcox(final)
lambda <- boxcoxplot$x[which.max(boxcoxplot$y)]
lambda
boxcoxplot
#trial and error, since lambda is close to 0 took the log of both x and y to find residual plot
logx <- log(waterdatnew$Temps)
logy <- log(waterdatnew$Pressures)
final2 <- lm(logy ~ logx, data = waterdatnew)
res2 <- resid(final2)
plot(fitted(final2),res2) + abline(0,0)
summary(final2)
plot(log(waterdatnew$Temps), log(waterdatnew$Pressures)) + abline(lm(log(waterdatnew$Pressures) ~ log(waterdatnew$Temps)))

#confident intervals 
install.packages("Rmisc")
library(Rmisc)
confint(final,'Temps',level=0.95)

#slope CI
lowerslopeCI <-  0.440282 - 1.697*0.03407
upperslopeCI <-  0.440282 + 1.697*0.03407
lowerslopeCI
upperslopeCI

#intercept CI  
lowerinterceptCI <- -18.32959 - 1.697*0.17906
upperinterceptCI <- -18.32959 + 1.697*0.17906
lowerinterceptCI
upperinterceptCI
  
#predicting the pressure
predictedp <- function(temp) {
 ans = (0.440282*temp) - 64.412751
 print(ans)
}

predictedp2 <- function(temp) {
  new = log(temp)
  ans = (4.05488*new) - 18.32959
  lateans = exp(ans)
  print(lateans)
}
predictedp(185)
predictedp2(185)






