rm(list=ls(all=TRUE))
library(readxl)
data = read.csv("LRdata.csv")
head(data)
y = data$price
x1 = data$engine.size
x2 = data$horsepower
fit = lm(y~x1+x2-1)
n = length(x1)
eb = sample(fit$residuals,n,replace = T)
b1 = as.numeric(fit$coefficients[1])
b2 = as.numeric(fit$coefficients[2])
yb = b1*x1 + b2*x2 + eb
bfit = lm(yb~x1+x2-1)
bfit
summary(bfit)
confint(bfit)
widthb1 = as.numeric(confint(fit)[3] - confint(fit)[1])
widthb2 = as.numeric(confint(fit)[4] - confint(fit)[2])
widthb1
widthb2
