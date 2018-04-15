#Hypothesis testing, Tests in OLS
#hourly wage

#female (1 = female)

#non-white (1= non-white)

#union (1 = unionized)

#education (years of education)

#experience (years of work experience)

#age

setwd("~/Teaching/Spring 2017/Yeshiva/Lab 7")
wagedata<-read.csv("wage.csv")
RegOLS<-lm(wage~female+nonwhite+unionmember+education+experience, data=wagedata)
#Coefficients
b<-coef(RegOLS)
#Variance Covariance
Vb<- vcov(RegOLS)

#Example 1 HA: “A female worker earns $3 less than male worker”
Rmat<-matrix(c(0, 1, 0, 0, 0, 0),nrow=1)
q<- -3
J<-nrow(Rmat)
n<-nrow(wagedata)
Fstat<-(1/J)* t(Rmat %*% b-q) %*% solve(Rmat %*% Vb %*% t(Rmat)) %*% (Rmat%*%b-q)
Fstat
k<-ncol(Vb)
pval<-1-pf(Fstat,J,n-k)
pval
#So we fail to reject the null hypothesis.

#Example 2 HA: “One year of education is worth eight years of experience”
Rmat<-matrix(c(0, 0, 0, 0, 1, -8),nrow=1)
q<- 0
Fstat<-(1/J)* t(Rmat %*% b-q) %*% solve(Rmat %*% Vb %*% t(Rmat)) %*% (Rmat%*%b-q)
Fstat
pval<-1-pf(Fstat,J,n-k)
pval
#So we fail to reject the null.

#Example 3 HA: “All of the above, plus”A unionized worker earns $1 more than a nonunionized worker“
Rmat1<-matrix(c(0, 1, 0, 0, 0, 0),nrow=1)
Rmat2<-matrix(c(0, 0, 0, 0, 1, -8),nrow=1)
Rmat3<-matrix(c(0, 0, 0, 1, 0, 0),nrow=1)
Rmat<-rbind(Rmat1,Rmat2,Rmat3)
q<- matrix(c(-3,0,1),nrow=3)
J<-nrow(Rmat)
Fstat<-(1/J)* t(Rmat %*% b-q) %*% solve(Rmat %*% Vb %*% t(Rmat)) %*% (Rmat%*%b-q)
Fstat
pval<-1-pf(Fstat,J,n-k)
pval


