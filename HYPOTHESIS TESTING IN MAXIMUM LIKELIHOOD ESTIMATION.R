#This data set comprises 553 observations of NV annual fishing license holders’ per-trip expenditures to destinations along the Truckee, Carson, and Walker Rivers in 2005 and 2006.
#Assume you want to test if experience and income have a different effect on expenditures for fly- fishers than spin casters. This requires the specification of an unconstrained model that allows the coeffcients for these 2 variables to differ across the two groups (similar to a partial Chow test). We can use the output from this model to perform a Wald test.
#load data
setwd("C:/Users/wyr/Desktop")
data<-read.csv("fishing.csv")
attach(data)
#prepare data
y<-expenditure
sel<-y==0
y[sel]<-1
y<-log(y)
n<-nrow(data)
fly <-matrix(flyfish,nrow=n )
spin<-matrix(1-(flyfish))
# Compose X such that the common coefficients come first,
# followed by the type-specific stuff
int1<-matrix(rep(fly,2),nrow=n)*cbind(fishyrs,lninc)
int2<-matrix(rep(spin,2),nrow=n)*cbind(fishyrs,lninc)
X<-cbind(rep(1,n),gender,age,age2,fly,int1,int2)
k<-ncol(X)
#Now, let’s make the function for optimizing the MLE:
CLRMllfan<-function(x,y,X,n,k){
bm<-x[1:k]
sig2<-x[k+1]^2 #square to keep positive
llf<- -(n/2)*log(2*pi)-(n/2)*log(sig2)-((1/(2*sig2))*t(y-X%*%bm)%*%(y-X%*%bm))
#Gradient
g1<- (t(X)%*%(y-X%*%bm))/sig2
g2<- -(n/(2*sig2))+((t(y-X%*%bm)%*%(y-X%*%bm))/((2*sig2^2)))
g<- rbind(g1,g2)
#Hessian
H1<- -(t(X)%*%X)/sig2
H2<- -(t(X)%*%(y-X%*%bm))/(sig2^2)
H3<- t(H2)
H4<- n/(2*sig2^2)-(t(y-X%*%bm)%*%(y-X%*%bm)/sig2^3)
H<-rbind(cbind(H1, H2),cbind(H3, H4))
return (list(llf,g,H))
} 
#Now, let’s get the starting values from OLS:
OLS<- lm(y~X[,2:k])
bols<-coef(OLS)
e<-resid(OLS)
s2<-(t(e)%*%e)/(n-k)
x0<-0.7*c(bols,s2)
#Now we perform the optimization:
cri<-10 #initial setting for convergence criterion
cri1<-0.0001 #convergence criterion
maxiter<-1000 #max. number of allowed iterations
stsz<-0.1 #step size, something between 0.1 and 1
#
b<-x0
jj<-0
#
while ((cri>cri1) & (jj<maxiter)) {
jj=jj+1
int<-CLRMllfan(b,y,X,n,k)
llf<-int[[1]]
g<-int[[2]]
H<-int[[3]]
cri<-sum(abs(g)) #evaluate convergence criterion
db=solve(-H)%*%g; #get directional vector
b<- b+stsz*db; #update b
iter<-c(jj, llf, cri)
# print(iter) #send iteration results to R's command window
if (jj==maxiter) {
    "Maximum number of iterations reached"
    break}
  #
} #end of "while"-loop
#Let’s capture the results
bm<-b #this includes sigma
sig2<-bm[k+1]^2
sem<-sqrt(diag(solve(-H))) #note here we need the negative H
tm<- bm/sem
ttmle<-data.frame(col1=c("constant","gender","age","age2","fly","fishyrs*fly",
                         "lninc*fly","fishyrs*spin","lninc*spin","sigma"),
col2=bm,
col3=sem,
col4=tm)
colnames(ttmle)<-c("variable","estimate","s.e.","t")
llf_u<-llf

ttmle
#Now we go ahead and do the Wald test:
invH<-solve(-H)
Vb<-invH[1:k,1:k] #we can use any of the ususal estimators for Vb for the Wald test;
b<-bm[1:k] # here we'll stick to the inverted Hessian
Rmat1<-matrix(c(0, 0, 0, 0, 0, 1, 0, -1, 0),nrow=1)
Rmat2<-matrix(c(0, 0, 0, 0, 0, 0, 1, 0, -1),nrow=1)
Rmat<-rbind(Rmat1,Rmat2)
q<- matrix(c(0,0),nrow=2)
J<-nrow(Rmat)
W<-t(Rmat %*% b-q) %*% solve(Rmat %*% Vb %*% t(Rmat)) %*% (Rmat%*%b-q)
pval=1-pchisq(W,J)

pval