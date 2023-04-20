library(tidyverse)
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(dbplyr)
library(tidyr)
library(VGAM)
library(MASS)
#Homework 5 - importing data 
lifetable <- read.csv("LifeTable.txt", header= T, sep = " ")
lifetable
dim(lifetable)

#Fit a cumulative logit model to the data and provide a table showing a summary of the fitted model

#1a. model using proportional odds
model1 <- vglm(cbind(y1,y2,y3,y4,y5)~gender+race, family=cumulative(parallel=TRUE),data=lifetable)
summary(model1)

#


# using vglm using non proporitonal odds
model2 <- vglm(cbind(y1,y2,y3,y4,y5)~gender+race, family=cumulative(parallel= FALSE),data=lifetable)
summary(model2)

#comparing the two models 
#anova of model
library(stats)
anova(model1,model2)
anova(model2)
#deviance difference between two models
difference <- deviance(model1) - deviance(model2)
difference
#degrees of freedom
df <- df.residual(model1) - df.residual(model2)
df

#Obtaining the p-value 
test <-pchisq(difference,df,lower.tail = FALSE)
test










