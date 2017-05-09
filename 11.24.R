financial.data<- read.table("T11-4.DAT")
head(financial.data)
#install.packages("gridExtra")
#require(lattice)
#require(gridExtra)

n<- nrow(financial.data)
n1<- 21
n2<- 25
# 
# par(mfrow=c(3,1))
# 
# a<- xyplot(V1~V2, financial.data, groups=V5)
# b<- xyplot(V1~V3, financial.data, groups=V5)
# c<- xyplot(V1~V4, financial.data, groups=V5)
# 
# grid.arrange(a,b,c, nrow=3)

# (b)

V1.bankrupt <- mean(financial.data$V1[1:21])
V2.bankrupt <- mean(financial.data$V2[1:21])
V3.bankrupt <- mean(financial.data$V3[1:21])
V4.bankrupt <- mean(financial.data$V4[1:21])

V1.nonbankrupt <- mean(financial.data$V1[22:46])
V2.nonbankrupt <- mean(financial.data$V2[22:46])
V3.nonbankrupt <- mean(financial.data$V3[22:46])
V4.nonbankrupt <- mean(financial.data$V4[22:46])


x1.bar <- as.matrix(c(V1.bankrupt, V2.bankrupt))
x2.bar <- as.matrix(c(V1.nonbankrupt, V2.nonbankrupt))

S1 <- cov(financial.data[1:21,1:2])
S2 <- cov(financial.data[22:46,1:2])

S1.inv <- solve(S1)
S2.inv <- solve(S2)

# (c)

k = (1/2)*log(det(S1)/det(S2)) + (1/2)*(t(x1.bar)%*%S1.inv%*%x1.bar - t(x2.bar)%*%S2.inv%*%x2.bar)
k

model1 <- matrix(NA, nrow=n, ncol=3)

for(i in 1:n)
{
  x0 <- rbind(financial.data[i,1],financial.data[i,2])
  a <- -(1/2)*t(x0)%*%(S1.inv - S2.inv)%*%x0 + (t(x1.bar)%*%S1.inv - t(x2.bar)%*%S2.inv)%*%x0 - k
  model1[i,1] <- financial.data[i,1]
  model1[i,2]<- financial.data[i,2]
  
  if(a>=0)
  {
    model1[i,3]<- 0 
  }
  else
  {
    model1[i,3]<- 1
  }
  
}
model1

# (d)

APER.1 <- sum(model1[,3]!=financial.data[,5])/(n1+n2)
APER.1

# (e)

p1 <- 0.05
p2 <- 0.95


model2 <- matrix(NA, nrow=46, ncol=3)

for(i in 1:n)
{
  x0 <- rbind(financial.data[i,1],financial.data[i,2])
  a <- -(1/2)*t(x0)%*%(S1.inv - S2.inv)%*%x0 + (t(x1.bar)%*%S1.inv - t(x2.bar)%*%S2.inv)%*%x0 - k
  model2[i,1] <- financial.data[i,1]
  model2[i,2]<- financial.data[i,2]
  
  if(a>=log(0.95/0.05))
  {
    model2[i,3]<- 0 
  }
  else
  {
    model2[i,3]<- 1
  }
  
}
model2

APER.2 <- sum(model2[,3]!=financial.data[,5])/(n1+n2)
APER.2

# (h)


#-- repeating part b for all 4 columns 

m=5

V1.bankrupt <- mean(financial.data$V1[1:21])
V2.bankrupt <- mean(financial.data$V2[1:21])
V3.bankrupt <- mean(financial.data$V3[1:21])
V4.bankrupt <- mean(financial.data$V4[1:21])

V1.nonbankrupt <- mean(financial.data$V1[22:46])
V2.nonbankrupt <- mean(financial.data$V2[22:46])
V3.nonbankrupt <- mean(financial.data$V3[22:46])
V4.nonbankrupt <- mean(financial.data$V4[22:46])

x1.bar <- as.matrix(c(V1.bankrupt, V2.bankrupt,V3.bankrupt, V4.bankrupt))
x2.bar <- as.matrix(c(V1.nonbankrupt, V2.nonbankrupt,V3.nonbankrupt, V4.nonbankrupt))

S1 <- cov(financial.data[1:21,1:(m-1)])
S2 <- cov(financial.data[22:46,1:(m-1)])

S1.inv <- solve(S1)
S2.inv <- solve(S2)

#-- repeating part c for all 4 columns 


k = (1/2)*log(det(S1)/det(S2)) + (1/2)*(t(x1.bar)%*%S1.inv%*%x1.bar - t(x2.bar)%*%S2.inv%*%x2.bar)


model3 <- matrix(NA, nrow=n, ncol=m)

for(i in 1:n)
{
  x0 <- rbind(financial.data[i,1],financial.data[i,2], financial.data[i,3], financial.data[i,4])
  a <- -(1/2)*t(x0)%*%(S1.inv - S2.inv)%*%x0 + (t(x1.bar)%*%S1.inv - t(x2.bar)%*%S2.inv)%*%x0 - k
  model3[i,1] <- financial.data[i,1]
  model3[i,2] <- financial.data[i,2]
  model3[i,3] <- financial.data[i,3]
  model3[i,4] <- financial.data[i,4]
  
  if(a>=log(1))
  {
    model3[i,5]<- 0 
  }
  else
  {
    model3[i,5]<- 1
  }
  
}
model3



# repeating part (d) for all columns

APER.3 <- sum(model3[,5]!=financial.data[,5])/(n1+n2)
APER.3

# repeating part (e) for all columns

p1 <- 0.05
p2 <- 0.95


model4 <- matrix(NA, nrow=46, ncol=m)

for(i in 1:n)
{
  x0 <- rbind(financial.data[i,1],financial.data[i,2], financial.data[i,3], financial.data[i,4])
  a <- -(1/2)*t(x0)%*%(S1.inv - S2.inv)%*%x0 + (t(x1.bar)%*%S1.inv - t(x2.bar)%*%S2.inv)%*%x0 - k
  model4[i,1] <- financial.data[i,1]
  model4[i,2] <- financial.data[i,2]
  model4[i,3] <- financial.data[i,3]
  model4[i,4] <- financial.data[i,4]
  
  if(a>=log(p2/p1))
  {
    model4[i,5]<- 0 
  }
  else
  {
    model4[i,5]<- 1
  }
  
}
model3


APER.4 <- sum(model4[,5]!=financial.data[,5])/(n1+n2)
APER.4
