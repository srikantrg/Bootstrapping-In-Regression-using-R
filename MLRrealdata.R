rm(list=ls(all=TRUE))
library(readxl)
data = read.csv("LRdata.csv")
head(data)
y = data$price
x1 = data$engine.size
x2 = data$horsepower
fit = lm(y~x1+x2-1)
fit
summary(fit)
confint(fit)
widthb1 = as.numeric(confint(fit)[3] - confint(fit)[1])
widthb2 = as.numeric(confint(fit)[4] - confint(fit)[2])
widthb1
widthb2
