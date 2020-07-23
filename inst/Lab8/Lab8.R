
myfun=function(X=6) X^2
myfun(3)
myfun()


myclt=function(n,iter,a=0,b=5){
y=runif(n*iter,a,b)
data=matrix(y,nr=n,nc=iter,byrow=TRUE)
sm=apply(data,2,sum)
h=hist(sm,plot=FALSE)
hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
sm
}
w=myclt(n=50,iter=10000,a=5,b=10)


################### uniform ##########################
### CLT uniform 
## my Central Limit Function
## Notice that I have assigned default values which can be changed when the function is called
mycltu=function(n,iter,a=0,b=10){
## r-random sample from the uniform
y=runif(n*iter,a,b)
## Place these numbers into a matrix
## The columns will correspond to the iteration and the rows will equal the sample size n
data=matrix(y,nr=n,nc=iter,byrow=TRUE)
## apply the function mean to the columns (2) of the matrix
## these are placed in a vector w
w=apply(data,2,mean)
## We will make a histogram of the values in w
## How high should we make y axis?
## All the values used to make a histogram are placed in param (nothing is plotted yet)
param=hist(w,plot=FALSE)
## Since the histogram will be a density plot we will find the max density

ymax=max(param$density)
## To be on the safe side we will add 10% more to this
ymax=1.1*ymax
## Now we can make the histogram
hist(w,freq=FALSE,  ylim=c(0,ymax), main=paste("Histogram of sample mean",
"\n", "sample size= ",n,sep=""),xlab="Sample mean")
## add a density curve made from the sample distribution
lines(density(w),col="Blue",lwd=3) # add a density plot
## Add a theoretical normal curve 
curve(dnorm(x,mean=(a+b)/2,sd=(b-a)/(sqrt(12*n))),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
## Add the density from which the samples were taken
curve(dunif(x,a,b),add=TRUE,lwd=4)

}
mycltu(n=20,iter=100000)

##############################  Binomial #########


## CLT Binomial
## CLT will work with discrete or continuous distributions 
## my Central Limit Function
## Notice that I have assigned default values which can be changed when the function is called

mycltb=function(n,iter,p=0.5,...){

## r-random sample from the Binomial
y=rbinom(n*iter,size=n,prob=p)
## Place these numbers into a matrix
## The columns will correspond to the iteration and the rows will equal the sample size n
data=matrix(y,nr=n,nc=iter,byrow=TRUE)
## apply the function mean to the columns (2) of the matrix
## these are placed in a vector w
w=apply(data,2,mean)
## We will make a histogram of the values in w
## How high should we make y axis?
## All the values used to make a histogram are placed in param (nothing is plotted yet)
param=hist(w,plot=FALSE)
## Since the histogram will be a density plot we will find the max density

ymax=max(param$density)
## To be on the safe side we will add 10% more to this
ymax=1.1*ymax

## Now we can make the histogram
## freq=FALSE means take a density
hist(w,freq=FALSE,  ylim=c(0,ymax),
main=paste("Histogram of sample mean","\n", "sample size= ",n,sep=""),
xlab="Sample mean",...)
## add a density curve made from the sample distribution
#lines(density(w),col="Blue",lwd=3) # add a density plot
## Add a theoretical normal curve 
curve(dnorm(x,mean=n*p,sd=sqrt(p*(1-p))),add=TRUE,col="Red",lty=2,lwd=3) 

}


mycltb(n=5,iter=10000,p=0.5)


####### Poisson ######################

## CLT Poisson
## CLT will work with discrete or continuous distributions 
## my Central Limit Function
## Notice that I have assigned default values which can be changed when the function is called

mycltp=function(n,iter,lambda=10,...){

## r-random sample from the Poisson
y=rpois(n*iter,lambda=lambda)
## Place these numbers into a matrix
## The columns will correspond to the iteration and the rows will equal the sample size n
data=matrix(y,nr=n,nc=iter,byrow=TRUE)
## apply the function mean to the columns (2) of the matrix
## these are placed in a vector w
w=apply(data,2,mean)
## We will make a histogram of the values in w
## How high should we make y axis?
## All the values used to make a histogram are placed in param (nothing is plotted yet)
param=hist(w,plot=FALSE)
## Since the histogram will be a density plot we will find the max density

ymax=max(param$density)
## To be on the safe side we will add 10% more to this
ymax=1.1*ymax

## Make a suitable layout for graphing
layout(matrix(c(1,1,2,3),nr=2,nc=2, byrow=TRUE))

## Now we can make the histogram
hist(w,freq=FALSE,  ylim=c(0,ymax), col=rainbow(max(w)),
main=paste("Histogram of sample mean","\n", "sample size= ",n," iter=",iter," lambda=",lambda,sep=""),
xlab="Sample mean",...)
## add a density curve made from the sample distribution
#lines(density(w),col="Blue",lwd=3) # add a density plot
## Add a theoretical normal curve 
curve(dnorm(x,mean=lambda,sd=sqrt(lambda/n)),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve

# Now make a new plot
# Since y is discrete we should use a barplot
barplot(table(y)/(n*iter),col=rainbow(max(y)), main="Barplot of sampled y", ylab ="Rel. Freq",xlab="y" )
x=0:max(y)
plot(x,dpois(x,lambda=lambda),type="h",lwd=5,col=rainbow(max(y)),
main="Probability function for Poisson", ylab="Probability",xlab="y")
}


mycltp(n=10,iter=10000)