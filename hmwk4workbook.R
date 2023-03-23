library(tidyverse)
library(readr)
library(dplyr)
library(tibble)
library(ggplot2)
library(dbplyr)
library(tidyr)

examplex <- read_delim("ApplStat-ExampleX.dat")
examplex

#Counts of students that intend to go to college, grouped by SES
onea = examplex %>% group_by(SES) %>% summarize(totalsum = sum(CollegeYes), )
onea
SES = factor(onea$SES, levels = c("L","LM","UM","H"))
ggplot(data = onea, aes(x = reorder(SES, desc(totalsum)), y = totalsum)) +geom_col(position = position_dodge()) + labs(title="College Yes Vs. SES", x="SES", y= "# of Students") 

# Counts of all students, grouped by SES
oneb = examplex %>% group_by(SES) %>% mutate(totaloneb = CollegeYes + CollegeNo) 
onebb= oneb %>% group_by(SES) %>% summarize(totall = sum(totaloneb))
onebb
ggplot(data = onebb, aes(x = reorder(SES, desc(totall)), y = totall)) +geom_col(position = position_dodge()) + labs(title="All Students Vs. SES", x="SES", y= "# of Students")

#Counts of students, grouped by SES and College plans, i.e. pairs of side-by-side bars for Yes and No, one pair for each of Low, Lower Middle, Upper Mid- dle and High SES. Such a plot is sometimes called a grouped bar plot
#need to take the mean of the yes and no of each SES 
examplex
examplex$SES <- as.factor(examplex$SES)
nosum <- tapply(examplex$CollegeNo, list(SES = examplex$SES), FUN = mean)
yessum <- tapply(examplex$CollegeYes, list(SES = examplex$SES), FUN = mean)
CollegeNo <- nosum
CollegeYes <- yessum
yesno <- t(as.matrix(data.frame(CollegeYes, CollegeNo)))

barplot(yesno, main="Student SES Groups and College Plans", xlab="Socioeconomic Status", ylab = "Avg # of Students", legend = c(rownames(yesno)), col=c("red","blue"), beside=TRUE) 


#factored bc theyre categorical variables 
examplex$SES <- factor(examplex$SES, levels = c("L", "LM", "UM", "H"))
examplex$IQ <- factor(examplex$IQ, levels = c("L", "LM", "UM", "H"))
examplex$Parent <- factor(examplex$Parent, levels = c("L", "H"))

#Fit logistic models to the data, with “College plans” as the binary response variable, and IQ, parental encouragement and SES as explanatory variables
examplex
cpmodel= glm(formula = cbind(CollegeYes, CollegeNo) ~ IQ + Parent + SES, family = binomial(link = "logit"), data = examplex)

summary(cpmodel)
anova(cpmodel)

#fit full logistic model with interactions
cpmodel2 = glm(formula = cbind(CollegeYes,CollegeNo) ~ IQ + Parent + SES + IQ:Parent + IQ:SES + Parent:SES + IQ:Parent:SES, family = binomial(link = "logit"), data = examplex)
summary(cpmodel2)

#anova of model
library(stats)
anova(cpmodel,cpmodel2)

#deviance difference between two models
difference <- deviance(cpmodel) - deviance(cpmodel2)
difference

#degrees of freedom
df <- df.residual(cpmodel) - df.residual(cpmodel2)
df

#Obtaining the p-value 
test <-pchisq(difference,df,lower.tail = FALSE)
test

#Using the main effects model, estimate the factor by which the `Yes/No’ ratio is increased for high IQ compared with a low IQ. Give also a 95% confidence interval.
#coefficient of IQH
cpmodel$coefficients[["IQH"]]

# the exponential term finds the odd ratio 
iqh <- exp(cpmodel$coefficients[["IQH"]])
round(iqh, 3)
confint <- confint(cpmodel, "IQH", 0.95)
round(exp(confint), 3)

anova(cpmodel)

res <- resid(cpmodel)
qqnorm(res, main="QQPlot") + qqline(res, lty=2) 
plot(fitted(cpmodel), res, main="Residual Plot") + abline(0,0)
