## Cheatsheet/fast reference for R functions

## A handy tabulation function. I has the abiklity to generate proportions of both columns and rows as well as calcualte chi squared, Fisher and McNemar statistics
rm(list=ls())
library(gmodels)
?CrossTable
## An example comes from Hilbe 2011, pg 17, loading titanic survival data:
library(COUNT)
data(titanic)
attach(titanic)
CrossTable(survived,class,prop.t=F,prop.r=F,prop.c=T,prop.chisq = F)

titanic$class<-relevel(factor(titanic$class),ref=3)
glm(survived~factor(class),family=poisson,data=titanic)
detach(titanic)

library(COUNT)
data(titanic)
attach(titanic)
library(gmodels)
CrossTable(survived,age,prop.t=T,prop.r=F,prop.c=F,prop.chisq = F)

mu<-2
y<-0:10
alpha<-1.5
amu<-mu*alpha
poisson.f<-(exp(-mu)*mu^y)/factorial(y)

nb.f<-exp(y*log(amu/(1+amu))
          -(1/alpha)*log(1+amu)
          +log(gamma(y+1/alpha))
          -log(gamma(y+1))
          -log(gamma(1/alpha))
)
plot(y,nb.f,col="red",pch=5,
     main="Poisson vs Negative Binomial PDFs")
lines(y,nb.f,col="red")
points(y,poisson.f,col="blue",pch=2)
lines(y,poisson.f,col="blue")
legend(4.3,.4,c("Negative Binomial: mean=2, a=1.5","Poisson: mean=2"),
       col=(c("red","blue")),
       pch=c(5,2),
       lty=1)
