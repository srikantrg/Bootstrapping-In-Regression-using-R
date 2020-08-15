rm(list=ls(all=TRUE))

library(datarium)


Y=marketing$sales
X1=marketing$youtube
X2=marketing$facebook
X3=marketing$newspaper


DF=data.frame(y=Y,x1=X1,x2=X2,x3=X3)

FULL=lm(y~.,data=DF)
FIT=step(FULL)
summary(FIT)

r=FIT$residuals
acf(r,main = "Auto Correlation Function of the Fitted Residuals")
n=length(r)

par(mfrow=c(1,2))
hist(r,probability = TRUE,main = "Histogram of Fitted residuals",xlab = "residuals");qqnorm(r)

confint(FIT)

b0=as.numeric(FIT$coefficients[1])
b1=as.numeric(FIT$coefficients[2])
b2=as.numeric(FIT$coefficients[3])

b0b=b1b=b2b=rep()
for(k in 1:1000)
{
  es=sample(r,n,replace = TRUE)
  ys=b0+b1*X1+b2*X2+es
  bootfit=lm(ys~X1+X2)
  b0b[k]=as.numeric(bootfit$coefficients[1])
  b1b[k]=as.numeric(bootfit$coefficients[2])
  b2b[k]=as.numeric(bootfit$coefficients[3])
}

mean(b0b);mean(b1b);mean(b2b)
quantile(b0b,c(0.025,0.975))
quantile(b1b,c(0.025,0.975))
quantile(b2b,c(0.025,0.975))

sd(b0b)/sqrt(n);sd(b1b)/sqrt(n);sd(b2b)/sqrt(n)


par(mfrow=c(1,2))
hist(b0b,main = expression(paste("Bootstrap distribution of ",beta[0])),probability = TRUE,xlab = expression(beta[0]));qqnorm(b0b)

par(mfrow=c(1,2))
hist(b1b,main = expression(paste("Bootstrap distribution of ",beta[1])),probability = TRUE,xlab = expression(beta[1]));qqnorm(b1b)

par(mfrow=c(1,2))
hist(b2b,main = expression(paste("Bootstrap distribution of ",beta[2])),probability = TRUE,xlab = expression(beta[2]));qqnorm(b2b)
