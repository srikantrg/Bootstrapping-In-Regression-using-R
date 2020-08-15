rm(list=ls(all=TRUE))
library(readxl)
data = read.csv("LRdata.csv")
head(data)
y = data$price
x = data$engine.size
fit = lm(y~x-1)
b1 = as.numeric(fit$coefficients[1])
n  = length(x)
es = sample(fit$residuals,n,replace = T)
yb = b1*x + es
bfit = lm(yb~x-1)
bfit
summary(bfit)
confint(bfit)
width = as.numeric(confint(bfit)[2] - confint(bfit)[1])
width
