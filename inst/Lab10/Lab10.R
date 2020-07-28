# maximum likelihood for univariate likelihoods

# various log likelihoods defined
logbin=function(x,param) log(dbinom(x,prob=param,size=10))
logpoiss=function(x,param) log(dpois(x,lambda=param)) 
logexp=function(x,param) log(dexp(x,rate=param))

#max likelihood function  
## For repeated sampling from same distribution                     
mymaxlik=function(lfun,x,param,...){
# how many param values are there?
np=length(param)
# outer -- notice the order, x then param
# this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
z=outer(x,param,lfun)
# z is a matrix where each x,param is replaced with the function evaluated at those values
y=apply(z,2,sum)

# y is a vector made up of the column sums
# Each y is the log lik for a new parameter value
plot(param,y,col="Blue",type="l",lwd=2,...)
# which gives the index for the value of y == max.
# there could be a max between two values of the parameter, therefore 2 indices
# the first max will take the larger indice
i=max(which(y==max(y)))
abline(v=param[i],lwd=2,col="Red")

# plots a nice point where the max lik is
points(param[i],y[i],pch=19,cex=1.5,col="Black")
axis(3,param[i],round(param[i],2))
#check slopes. If it is a max the slope shoud change sign from + to 
# We should get three + and two -vs
ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}

mymaxlik(x=c(9,9,1,9,9,9),param=seq(0,1,length=1000),lfun=logbin,xlab=expression(pi),main="Binomial",cex.main=2)
mymaxlik(x=c(3,4,3,5),param=seq(0,20,length=1000),lfun=logpoiss,xlab=expression(lambda),main="Poisson",cex.main=2)



#mymaxlik(x=c(3,4,5,2,3,4,5,5,5,1),param=seq(0,1,length=1000),lfun=logbin2)
###### Improving accuracy #####

logbin=function(x,param) log(dbinom(x,prob=param,size=10))
logpoiss=function(x,param) log(dpois(x,lambda=param)) 

                       
mymld=function(lfun,x,param,delta=0.0001){    # param = parameter values, delta=accuracy, x=data
z=outer(x,param,lfun)    # create outer product and evaluate at lfun
y=apply(z,2,sum) # x by param, 2=columns , sum columns = sum of log lik

i=max(which(y==max(y)))# the index for which y is biggest, if two then take the last one

param2=seq(param[i-2],param[i+2],by=delta)# The maximum will be between these two, increments by delta
zz=outer(x,param2,lfun) # new z, call it zz
yy=apply(zz,2,sum)   # new y, call it yy
ii=max(which(yy==max(yy)))# new i,  call it ii , if two, take max of them (last one)
layout(matrix(c(1,2),nr=1,nc=2,byrow=TRUE))# divide plotting space for two graphs
plot(param,y,col="Blue",type="l",lwd=2,ylab="Log. Lik.",xlab=expression(theta))# plot log lik Vs parameter values
abline(v=param[i],lwd=2,col="Red") # Show vertical line at estimated value          
axis(3,param[i],round(param[i],2))
points(param[i],y[i],pch=19,cex=1.5,col="Black")# Plot the point
plot(param2,yy,col="Blue",type="l",lwd=2,ylab="Log. Lik.",xlab=expression(theta),las=2) # construct new plot for refined estimate
abline(v=param2[ii],lwd=2,col="Red")  # new verical line
val=round(param2[ii],abs(log10(delta))) ## rounds to the nth place where n is st delta=10^-n.         
axis(3,param2[ii],val)
points(param2[ii],yy[ii],pch=19,cex=1.5,col="Black")
}

mymld(x=c(5,5,6,6,6,6),param=seq(0,1,length=1000),lfun=logbin,delta=0.000001)
mymld(x=c(3,4,3,5),param=seq(0,20,length=1000),lfun=logpoiss)

####### End of improving accuracy ######


## Joint density has one unknown common parameter but different values for the other parameters
## We need only make L(theta)

logbin2=function(theta){log(dbinom(1,prob=theta,size=2)) + log(dbinom(7,prob=theta,size=8))}

mymaxlikg=function(lfun="logbin2",theta) { # default log lik is a combination bin
nth=length(theta)  # nu. of valuse used in theta
thmat=matrix(theta,nr=nth,nc=1,byrow=TRUE) # Matrix of theta
z=apply(thmat,1,lfun) # z holds the log lik values
zmax=max(which(z==max(z)))  # finding the INDEX of the max lik
plot(theta,exp(z),type="l") # plot of lik
abline(v=theta[zmax],col="Blue")   #  verical line through max
axis(3,theta[zmax],round(theta[zmax],4))  # one tick on the third axis 
theta[zmax]   # theta corresponding to max lik
}

mymaxlikg(theta=seq(0,1,length=10000))

######################################### TWO parameters ##########################

# Make a max lik function for two parameters


mymlnorm=function(x,mu,sig,...){  #x sample vector
nmu=length(mu) # number of values in mu
nsig=length(sig)
n=length(x) # sample size
zz=c()    ## initialize a new vector
lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))   # log lik for normal
for(j in 1:nsig){
z=outer(x,mu,lfun,p=sig[j]) # z a matrix 
# col 1 of z contains lfun evaluated at each x with first value of mu, 
# col2 each x with 2nd value of m 
# all with sig=sig[j]
y=apply(z,2,sum)
# y is a vector filled with log lik values, 
# each with a difft mu and all with the same sig[j]
zz=cbind(zz,y)
## zz is the matrix with each column containing log L values, rows difft mu, cols difft sigmas 
}
maxl=max(exp(zz))
coord=which(exp(zz)==maxl,arr.ind=TRUE)
maxlsig=apply(zz,1,max)
contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
main=expression(paste("L(",mu,",",sigma,")",sep="")),...)
mlx=round(mean(x),2)  # theoretical
mly=round(sqrt((n-1)/n)*sd(x),2)
#axis(1,at=c(0:20,mlx),labels=sort(c(0:20,mlx)))
#axis(2,at=c(0:20,mly),labels=TRUE)
abline(v=mean(x),lwd=2,col="Green")
abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")

# Now find the estimates from the co-ords
muest=mu[coord[1]]
sigest=sig[coord[2]]

abline(v=muest, h=sigest)
return(list(x=x,coord=coord,maxl=maxl))
}

mymlnorm(x=c(5,7,7,8,10),mu=seq(5,10,length=1000),sig=seq(0.1,4,length=1000),lwd=2,labcex=1)



##### Beta distribution

mymlbeta=function(x,alpha,beta,...){  #x sample vector
na=length(alpha) # number of values in alpha
nb=length(beta)
n=length(x) # sample size
zz=c()    ## initialize a new vector
lfun=function(x,a,b) log(dbeta(x,shape1=a,shape2=b))   # log lik for beta
for(j in 1:nb){
z=outer(x,alpha,lfun,b=beta[j]) # z a matrix 
# col 1 of z contains lfun evaluated at each x with first value of alpha, 
# col2 each x with 2nd value of a 
# all with b=beta[j]
y=apply(z,2,sum)
# y is a vector filled with log lik values, 
# each with a difft alpha and all with the same sig[j]
zz=cbind(zz,y)
## zz is the matrix with each column containing log L values, rows difft alpha, cols difft betas 
}
maxl=max(exp(zz))    # max lik
coord=which(exp(zz)==maxl,arr.ind=TRUE)  # find the co-ords of the max
aest=alpha[coord[1]] # mxlik estimate of alpha
best=beta[coord[2]]
contour(alpha,beta,exp(zz),las=3,xlab=expression(alpha),ylab=expression(beta),axes=TRUE,
main=expression(paste("L(",alpha,",",beta,")",sep="")),...)

abline(v=aest, h=best)
points(aest,best,pch=19)
axis(4,best,round(best,2),col="Red")
axis(3,aest,round(aest,2),col="Red")
return(list(x=x,coord=coord,maxl=maxl,maxalpha=aest,maxbeta=best))
}

mymlbeta(x=rbeta(100,shape1=2,shape2=5),alpha=seq(1,4,length=100),beta=seq(2,8,length=100),lwd=2,labcex=1)

## Examine the variability of the maximum likelihood from sample to sample
## This could form the core of a new function 
## Boot Max Lik
layout(matrix(1:9,nr=3,nc=3,byrow=TRUE))
z=c()
a=3
b=4
sam= rbeta(30,shape1=a,shape2=b)
nsam=length(sam)
for(i in 1:9){
w=mymlbeta(x=sample(sam,nsam,replace=TRUE),alpha=seq(0.1,20,length=100),beta=seq(0.1,20,length=100),lwd=2,labcex=1,col="steelblue")
points(a,b,col="Red",pch=19)
abline(v=a,h=b,col="Red")
z<-rbind(z,c(w$maxalpha,w$maxbeta))
 }
colnames(z)=c("alpha","beta")
z
dz=apply(z-c(a,b),1,function(x) sqrt(sum(x^2)) )
plot(dz)
which(dz>4)


### 2 parameters with a general log  likelihood 

logbinpois=function(theta1,theta2) log(dbinom(5,size=10,prob=theta1)) + log(dbinom(7,size=8,prob=theta1))+ log(dpois(3,lambda=theta2))

maxlikg2=function(theta1,theta2,lfun="logbinpois",...){
n1=length(theta1)
n2=length(theta2)
z=outer(theta1,theta2,lfun)
contour(theta1,theta2,exp(z),...) # exp(z) gives the lik
maxl=max(exp(z))    # max lik
coord=which(exp(z)==maxl,arr.ind=TRUE)  # find the co-ords of the max
th1est=theta1[coord[1]] # mxlik estimate of theta1
th2est=theta2[coord[2]]
abline(v=th1est,h=th2est)
axis(3,th1est,round(th1est,2))
axis(4,th2est,round(th2est,2),las=1)
list(th1est=th1est,th2est=th2est)
}
maxlikg2(theta1=seq(0,1,length=1000),theta2=seq(0,10,length=1000),nlevels=20)
