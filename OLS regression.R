#OLS Regression
#Let’s use the X matrix we created, but since the lm function does not use a column of 1’s, then we have to set up a new matrix without the first column, we need to save the coefficients and the OLS estimator for σ2:
#σ^2=u′u/（n−k）
Xnew<-data.matrix(X[,2:k])
OLSE<-lm(y~Xnew)
summary(OLSE)

#SAVE THE PARAMETER VALUES FROM OLS TO USE AS INITIAL 
#GUESS IN MLE

BetaOLS<-OLSE$coefficients
u<-OLSE$residuals
SigSq<-(t(u)%*%u)/(n-k)

#set these as initial parameters
Init<-c(BetaOLS,SigSq)

#run the optimization
opt<-optim(Init,loglikReg,gr=NULL,y=y,X=X,n=n,k=k, 
           method="BFGS")

#Let's look at the parameters
opt$par
Init
opt$value