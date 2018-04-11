#sample size
n<-5000
#probabilities
prob<- c(0.2, 0.03, 0.05, 0.23, 0.05, 0.07, 0.17)
#Let's create a sample of credits 
credits<- sample(9:15, n, replace=TRUE, prob=prob)
#barplot
table(credits)
barplot(table(credits))

#Probabilities
probwork<-c(rep(0.045,10),rep(0.055,10))
#Work
work<-sample(1:20,n, replace=TRUE, prob=probwork)
#Look at the distribution
table(work)
barplot(table(work))

#Generate the variable
score<-rnorm(n, mean=1600, sd=200)
hist(score, prob=TRUE)
lines(density(score), col="blue")

#Upper
upper<-rbinom(n, 1, 0.45)

#plot this
table(upper)
barplot(table(upper))

#Disturbance
epsilon<-rnorm(n, mean=0, sd=1.7)
#plot
plot(density(epsilon))

#Simulating the dependent variable y=Xβ+ε
#Let us now define the coefficients we will use for this model
#Intercept
beta_0<- 1.2
#Credits
beta_1<- 0.03
#Work
beta_2<- -0.02
#SAT scores
beta_3<- 0.015
#Upper
beta_4<- 0.5
# Beta vector
beta<-c(beta_0, beta_1, beta_2, beta_3, beta_4)
X<-cbind(rep(1,n), credits, work, score, upper)
#So now the dependent variable is:
y=X%*%beta+epsilon

#Obtaining the parameter estimates β^OLS,Applying directly, we have:
betahat<-solve(t(X)%*%X)%*%t(X)%*%y
betahat
#This is close to the “true” values of β.

#Via the function lm in R,lm is a function that uses linear models,
#see the code below to call the function and then retrieve only the coefficients.
regression<-lm(y~X)
#Which matches our use of the analytical formula.
