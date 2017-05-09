
#install.packages("MASS")
library(MASS)
set.seed(0)
n=100
Sigma=rbind(c(1, 0.5), c(0.5, 1))
Sigma.inv = solve(Sigma)
mu1<- c(-1,1)
mu2 <- c(-1,2)
ds1<- mvrnorm(n, mu1, Sigma=Sigma)

ds2<- mvrnorm(n, mu2, Sigma=Sigma)

ds3<- cbind(rcauchy(n, location = 0, scale = 1),rcauchy(n, location = 0, scale = 1))

mu1<- rbind(-1,1)
mu2 <- rbind(-1,2)
ds <- rbind(ds1,ds2,ds3)


classified <- matrix(NA, nrow=300, ncol = 3)
for( i in 1:300)
{
  x0 <- as.matrix(ds[i,])
  f1 <- (1/(2*pi))*det(Sigma)^(-1/2)*exp(-(1/2)*t(x0-mu1)%*%Sigma.inv%*%(x0-mu1))
  f2 <- (1/2*pi)*det(Sigma)^(-1/2)*exp(-(1/2)*t(x0-mu2)%*%Sigma.inv%*%(x0-mu2)) 
  f3 <- (1/(pi*(1+x0[1]^2)))*(1/(pi*(1+x0[2]^2)))
  
    classified[i,1]<- ds[i,1]
    classified[i,2]<- ds[i,2]
    classified[i,3]<- which(c(f1,f2,f3)==max(f1,f2,f3))
}

ds1<- cbind(ds1,1)
ds2<- cbind(ds2,2)
ds3<- cbind(ds3,3)
ds<- rbind(ds1,ds2,ds3)


sum(ds[,3]!=classified[,3])/300
