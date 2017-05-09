iris.data <- read.table("T11-5.DAT")
#--- log(y1) and log(y2)

data <- cbind(log(iris.data$V1/iris.data$V2), log(iris.data$V3/iris.data$V4))
head(data)

S1 <- cov(data[(1:50),(1:2)])
S2 <- cov(data[(51:100),(1:2)])
S3 <- cov(data[(101:150),(1:2)])

n1=50
n2=50
n3=50

S.pool <- (1/(n1+n2+n3-g))*(n1-1)*S1+(n2-1)*S2+(n3-1)*S3
S.pool.inv <- solve(S.pool)
g=3

x1.bar <- rbind(mean(data[,1][1:50]),mean(data[,2][1:50]))
x2.bar <- rbind(mean(data[,1][51:100]),mean(data[,2][51:100]))
x3.bar <- rbind(mean(data[,1][101:150]),mean(data[,2][101:150]))

p1=1/3
p2=1/3
p3=1/3

d1<-c()
d2<- c()
d3<- c()


classified.1 <- matrix(NA, nrow=150, ncol=3)
for(i in 1:150)
{
x<- as.matrix(data[i,])
d1[i]<- t(x1.bar)%*%S.pool.inv%*%x - (1/2)*t(x1.bar)%*%S.pool.inv%*%x1.bar + log(p1)
d2[i]<- t(x2.bar)%*%S.pool.inv%*%x - (1/2)*t(x2.bar)%*%S.pool.inv%*%x2.bar + log(p2)
d3[i]<- t(x3.bar)%*%S.pool.inv%*%x - (1/2)*t(x3.bar)%*%S.pool.inv%*%x3.bar + log(p3)

classified.1[i,] <- rbind(data[i,1], data[i,2], which(c(d1[i],d2[i],d3[i])==max(d1[i],d2[i],d3[i])) )

}

head(classified.1)

APER.1<- sum(iris.data[,5]!=classified.1[,3])/150
APER.1

#----- Log(y1)

data <- cbind(log(iris.data$V1/iris.data$V2))
head(data)

S1 <- var(data[(1:50),(1:1)])
S2 <- var(data[(51:100),(1:1)])
S3 <- var(data[(101:150),(1:1)])

n1=50
n2=50
n3=50

S.pool <- (1/(n1+n2+n3-g))*(n1-1)*S1+(n2-1)*S2+(n3-1)*S3
S.pool.inv <- solve(S.pool)
g=3

x1.bar <- rbind(mean(data[,1][1:50]))
x2.bar <- rbind(mean(data[,1][51:100]))
x3.bar <- rbind(mean(data[,1][101:150]))

p1=1/3
p2=1/3
p3=1/3

d1<-c()
d2<- c()
d3<- c()

classified.2 <- matrix(NA, nrow=150, ncol=2)
for(i in 1:150)
{
  x<- as.matrix(data[i,])
  d1<- t(x1.bar)%*%S.pool.inv%*%x - (1/2)*t(x1.bar)%*%S.pool.inv%*%x1.bar + log(p1)
  d2<- t(x2.bar)%*%S.pool.inv%*%x - (1/2)*t(x2.bar)%*%S.pool.inv%*%x2.bar + log(p2)
  d3<- t(x3.bar)%*%S.pool.inv%*%x - (1/2)*t(x3.bar)%*%S.pool.inv%*%x3.bar + log(p3)
  
  classified.2[i,] <- rbind(data[i,1], which(c(d1,d2,d3)==max(d1,d2,d3)) )
}

APER.2<- sum(iris.data[,5]!=classified.2[,2])/150
APER.2

#-- log(Y2)

data <- cbind(log(iris.data$V3/iris.data$V4))
head(data)

S1 <- var(data[(1:50),(1:1)])
S2 <- var(data[(51:100),(1:1)])
S3 <- var(data[(101:150),(1:1)])

n1=50
n2=50
n3=50

S.pool <- (1/(n1+n2+n3-g))*(n1-1)*S1+(n2-1)*S2+(n3-1)*S3
S.pool.inv <- solve(S.pool)
g=3

x1.bar <- rbind(mean(data[,1][1:50]))
x2.bar <- rbind(mean(data[,1][51:100]))
x3.bar <- rbind(mean(data[,1][101:150]))

p1=1/3
p2=1/3
p3=1/3

d1<-c()
d2<- c()
d3<- c()

classified.3 <- matrix(NA, nrow=150, ncol=2)
for(i in 1:150)
{
  x<- as.matrix(data[i,])
  d1<- t(x1.bar)%*%S.pool.inv%*%x - (1/2)*t(x1.bar)%*%S.pool.inv%*%x1.bar + log(p1)
  d2<- t(x2.bar)%*%S.pool.inv%*%x - (1/2)*t(x2.bar)%*%S.pool.inv%*%x2.bar + log(p2)
  d3<- t(x3.bar)%*%S.pool.inv%*%x - (1/2)*t(x3.bar)%*%S.pool.inv%*%x3.bar + log(p3)
  
  classified.3[i,] <- rbind(data[i,1], which(c(d1,d2,d3)==max(d1,d2,d3)) )
  
}

APER.3<- sum(iris.data[,5]!=classified.3[,2])/150
APER.3




