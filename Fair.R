#Set working directory
setwd("E:/Acads/UT Dallas/Econometrics/Moran Blueshtein/Homeworks/HW2")

#Load libraries
library(foreign)
library(ggplot2)

#Load data
data = read.dta('fair4.dta')
df1 <- subset(data , year > 1915 & year < 2005) 
plot(x = df1$growth, y = df1$vote, xlab = 'Growth', ylab = 'Vote')

lm1 = lm(vote~growth, data = df1)
#Plotting regression line
abline(lm1, col = 'blue')

#Load data
df1 <- subset(data , year > 1915) 
plot(x = df1$inflation, y = df1$vote, xlab = 'Inflation', ylab = 'Vote')

lm2 = lm(vote~inflation, data = df1)
#Plotting regression line
abline(lm2, col = 'blue')

