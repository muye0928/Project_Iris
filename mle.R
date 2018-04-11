#Maximum likelihood estimation
#install.packages("AER")
library(AER)
data("CPS1985")
summary(CPS1985)
head(CPS1985)
for(level in unique(CPS1985[,"married"])){
  CPS1985[paste("dummy_married", level, sep = "_")] <- ifelse(CPS1985[,"married"] == level, 1, 0)
}
#Look at the first observations of the the "yes dummy"
head(CPS1985$dummy_married_yes)
catego<-c("ethnicity", "gender")
for( i in 1:length(catego)){
  for(level in unique(CPS1985[,catego[i]])) {
    CPS1985[paste("dummy",catego[i], level, sep = "_")] <- ifelse(CPS1985[,catego[i]] == level, 1, 0)
  }
}
names(CPS1985)

#So now we have that we have all the dummies and we can now create a matrix of independent variables. We will leave one dummy out from all the categories so we don’t have to fear multicollinearity (full rank condition violation).
#We begin by creating a vector with the names of the dummies we don’t want and then proceed to create the independent and dependent variables.
destroy<-c("wage",
           "married","dummy_married_no",
           "ethnicity" ,"dummy_ethnicity_other", 
           "region",
           "gender", "dummy_gender_male",
           "occupation",
           "union",
           "sector")
#Let's get the number of observations
n<-nrow(CPS1985)
#Dependent
y<-log(CPS1985[,"wage"])
#Independent
X<-cbind(rep(1,n), CPS1985[,-which(colnames(CPS1985) %in% destroy)])

#Log likelihood function
#L(β,σ2)=−n2ln(2π)−n2lnσ2−12σ2(y−Xβ)′(y−Xβ)
k<-ncol(X)
loglikReg<-function(x,y,X,n,k){
  beta<-x[1:k]
  sig2<-x[(k+1)]^2    #square to keep positive
  X<-as.matrix(X)
  L<- -(n/2)*log(2*pi)-(n/2)*log(sig2)-((1/(2*sig2))*t((y-X%*%beta))%*%(y-X%*%beta)) 
  llik<-sum(L)
  return(-llik)
}