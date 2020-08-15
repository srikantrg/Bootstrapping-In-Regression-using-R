rm(list=ls(all=TRUE))
library(readxl)
data = read.csv("LRdata.csv")
head(data)
y = data$price
x = data$engine.size
fit = lm(y~x-1)
summary(fit)
confint(fit)
width = as.numeric(confint(fit)[2] - confint(fit)[1])
width

