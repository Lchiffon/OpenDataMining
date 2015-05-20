library(bayesm)
library(xlsx)
data<-"C:/Users/Administrator/Desktop/贝叶斯.xls"
mydata<-read.xlsx("C:/Users/Administrator/Desktop/贝叶斯.xls")

pibeta<-c(1,1,1,1,1)
betahat<-c(5.31,-7.06,3.5,-3.41,2.98)
cov<-matrix(c(5.6,-7.7,15.86,-2.24,-2.04,-7.7,35.73,-70.6,2.89,1.87,15.86,-70.6,
            185.18,-7.05,-0.68,-2.24,2.89,-7.05,1.45,-1.22,-2.04,1.87,-0.68,-1.22,
            9.41 ),nrow=5)
set.seed(1234)
beta0<-rnorm(50,betahat[1],sqrt(cov[1,1]))
Beta0<-sort(beta0)
p1<-dnorm(Beta0,betahat[1],sqrt(cov[1,1]))
plot(Beta0,p1,type="l")

set.seed(1234)
beta2<-rnorm(1000,betahat[3],sqrt(cov[3,3]))
Beta2<-sort(beta2)
p2<-dnorm(Beta2,betahat[3],sqrt(cov[3,3]))
plot(Beta2,p2,type="l")

library(MASS)
set.seed(1234)
beta<-mvrnorm(1000,betahat,cov) 
h<-det(cov)^-1/2
LBeta2<-sort(beta[,3])
pbeta2<-dnorm(LBeta2,betahat[3],sqrt(cov[3,3]))
p<-h*pbeta2/sum(h*pbeta2)
par(mfrow=c(1,2))
plot(LBeta2,p,type="l",xlab="Beta2",ylab="Density",col="BLUE")
plot(Beta2,p2,type="l")
lines(Beta2,p2,type="l",col="red")

              

