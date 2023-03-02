#Homework 3 workbook
library(readr)
library(tidyr)
library(dplyr)
sediment <- read.table("Sediment.txt", sep="|", header=T)
sediment 
#create boxplot 
library(ggplot2)
qplot(factor(Creek), Measurement, data = sediment, col=Side,geom = "boxplot") + labs(title="Figure 1: Measurements vs Creek", x="Creeks (A to F)", y= "Measurement")
sediment

#a. mean of measurements in each creak... side vs land t test 
a.sea = sediment %>% filter(Creek =="A", Side=="Sea") %>% select(Measurement)
a.land = sediment %>% filter(Creek =="A", Side=="Land") %>% select(Measurement)
b.sea = sediment %>% filter(Creek =="B", Side=="Sea") %>% select(Measurement)
b.land = sediment %>% filter(Creek =="B", Side=="Land") %>% select(Measurement)
c.sea = sediment %>% filter(Creek =="C", Side=="Sea") %>% select(Measurement)
c.land = sediment %>% filter(Creek =="C", Side=="Land") %>% select(Measurement)
d.sea = sediment %>% filter(Creek =="D", Side=="Sea") %>% select(Measurement)
d.land = sediment %>% filter(Creek =="D", Side=="Land") %>% select(Measurement)
e.sea = sediment %>% filter(Creek =="E", Side=="Sea") %>% select(Measurement)
e.land = sediment %>% filter(Creek =="E", Side=="Land") %>% select(Measurement)
f.sea = sediment %>% filter(Creek =="F", Side=="Sea") %>% select(Measurement)
f.land = sediment %>% filter(Creek =="F", Side=="Land") %>% select(Measurement)

#hypothesis test on means 
t.test(a.sea, a.land)
t.test(b.sea, b.land)
t.test(c.sea, c.land)
t.test(d.sea, d.land)
t.test(e.sea, e.land)
t.test(f.sea, f.land)

x <- c(1,2,3,4,5,6,7,8)
y <- c(1,2,3,4,5,6,7,8)
t.test(x,y )

#b. fitting regression line 
reg.model <- lm(Measurement ~ Side + Creek + (Creek*Side), data=sediment)
summary(reg.model)
anova(reg.model)
aov(reg.model)
#3a. combining land measurement 
land = sediment %>% filter(Side =="Land") %>% select(Distance, Measurement)
land
land.model = lm(Measurement ~ Distance, data=land)
summary(land.model)

#each creek land measurement 
all.land = sediment %>% filter(Side =="Land") %>% dplyr::select(Distance,Measurement)
mod = lm(Measurement ~ Distance, data=all.land)
mod = lm(log(Measurement)^2 ~ (Distance), data=all.land)
summary(mod)
res <- resid(mod)
plot(fitted(mod), res, main="Final Residual Plot: All Creeks") + abline(0,0) #resid not normal 
qqnorm(res, main="Final Q-Q Plot: All Creeks") + qqline(res, lty=2) #qq not normal 
#taking log and squared??

#boxcox prodecure 
library(MASS)
boxcoxplot <- boxcox(mod)
lambda <- boxcoxplot$x[which.max(boxcoxplot$y)]
lambda

#creeka,land
a.land1 = sediment %>% filter(Creek == "A", Side =="Land") %>% dplyr::select(Distance,Measurement)
a.mod = lm(Measurement^2 ~ Distance, data=a.land1)
summary(a.mod)
res <- resid(a.mod)
qqnorm(res, main="Final Q-Q Plot: Creek A") + qqline(res, lty=2) 
boxcoxplot <- boxcox(a.mod)
lambda <- boxcoxplot$x[which.max(boxcoxplot$y)]
lambda

#creek b land 
b.land1 = sediment %>% filter(Creek == "B", Side =="Land") %>% dplyr::select(Distance,Measurement)
b.mod = lm(Measurement ~ Distance, data=b.land1)
summary(b.mod)
res <- resid(b.mod)
qqnorm(res, main="Q-Q Plot: Creek B") + qqline(res, lty=2) 
boxcoxplot <- boxcox(b.mod)
lambda <- boxcoxplot$x[which.max(boxcoxplot$y)]
lambda

#creek c land 
c.land1 = sediment %>% filter(Creek == "C", Side =="Land") %>% dplyr::select(Distance,Measurement)
c.mod = lm(log(Measurement)~ Distance, data=c.land1)
summary(c.mod)
res <- resid(c.mod)
qqnorm(res, main="Final Q-Q Plot: Creek C") + qqline(res, lty=2) 
boxcoxplot <- boxcox(c.mod)
lambda <- boxcoxplot$x[which.max(boxcoxplot$y)]
lambda

#creek d land 
d.land1 = sediment %>% filter(Creek == "D", Side =="Land") %>% dplyr::select(Distance,Measurement)
d.mod = lm((Measurement)~ Distance, data=d.land1)
summary(d.mod)
res <- resid(d.mod)
qqnorm(res, main=" Q-Q Plot: Creek D") + qqline(res, lty=2) 
boxcoxplot <- boxcox(d.mod)
lambda <- boxcoxplot$x[which.max(boxcoxplot$y)]
lambda

#creek e land 
e.land1 = sediment %>% filter(Creek == "E", Side =="Land") %>% dplyr::select(Distance,Measurement)
e.mod = lm(log(Measurement) ~ Distance, data=e.land1)
summary(e.mod)
res <- resid(e.mod)
qqnorm(res, main="Final Q-Q Plot: Creek E") + qqline(res, lty=2) 
boxcoxplot <- boxcox(e.mod)
lambda <- boxcoxplot$x[which.max(boxcoxplot$y)]
lambda

#creek f land 
f.land1 = sediment %>% filter(Creek == "F", Side =="Land") %>% dplyr::select(Distance,Measurement)
f.mod = lm(log(Measurement) ~ Distance, data=f.land1)
summary(f.mod)
res <- resid(f.mod)
qqnorm(res, main="Final Q-Q Plot: Creek F") + qqline(res, lty=2) 
boxcoxplot <- boxcox(f.mod)
lambda <- boxcoxplot$x[which.max(boxcoxplot$y)]
lambda




