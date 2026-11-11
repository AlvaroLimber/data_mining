rm(list=ls())
data(iris)
bd<-iris[,-5]
names(bd)
bd
bd[1, ]
bd[2, ]
bd[3, ]

xi<-bd[2, ]
xj<-bd[3, ]
sqrt(sum((xi-xj)^2))#Euclideana
sum(abs(xi-xj))#Manhattan
xi<-t(xi)
xj<-t(xj)
sqrt(t(xi-xj)%*%solve(cov(bd))%*%(xi-xj))#Mahalanobis
