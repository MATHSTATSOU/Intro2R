set.seed(20);x=rnorm(20,mean=10,sd=sqrt(6))
set.seed(30);y=rnorm(20,mean=20,sd=sqrt(4))
v=var.test(x,y,conf.level=0.75)
ci=v$conf
ci[2]

t.test(x,mu=20,conf.level=0.95)

#Lab 12
# one sample t-test
set.seed(55);x1=rnorm(30,mean=25,sd=5)
boxplot(x1, main="Sample x1")

t.test(x1,mu=28)
ci=t.test(x1,mu=23)$conf.int
ci
abline(h=c(ci,mean(x1)),col=c("Red","Red","Green"))

#tcalc
tcalc=(mean(x1)-23)/(sd(x1)/sqrt(30))
tcalc


#### FUNCTION for Pvalues 
# Display P-value areas
mypvalue=function(t0,xmax=4,n=20, alpha=0.05){
#calculate alpha/2
va=round(pt(-t0,df=n-1),4)
pv=2*va

# plot the t dist
curve(dt(x,df=n-1),xlim=c(-xmax,xmax),ylab="T Density",xlab=expression(t),
main=substitute(paste("P-value=", pv, " alpha=", alpha)))


# set up points on the polygon to the right
xcurve=seq(t0,xmax,length=1000)
ycurve=dt(xcurve,df=n-1)

# set up points to the left
xlcurve=seq(-t0,-xmax,length=1000)
ylcurve=dt(xcurve,df=n-1)

# Shade in the polygon defined by the line segments
polygon(c(t0,xcurve,xmax),c(0,ycurve,0),col="green")
polygon(c(-t0,xlcurve,-xmax),c(0,ylcurve,0),col="green")

# make quantiles
q=qt(1-alpha/2,n-1)
abline( v=c(q,-q),lwd=2) # plot the cut off t value 
axis(3,c(q,-q),c(expression(abs(t[alpha/2])),expression(-abs(t[alpha/2]))))


# Annotation
text(0.5*(t0+xmax),max(ycurve),substitute(paste(area, "=",va)))
text(-0.5*(t0+xmax),max(ycurve),expression(area))

return(list(q=q,pvalue=pv))
}
##### END of FUNCTION for P VALUES    #####

mypvalue(tcalc,n=30,alpha=0.05)
t.test(x1,mu=23)


### bootstrap pvalues
bootpval<-function(x,conf.level=0.95,iter=3000,mu0=0, test="two"){
n=length(x)
y=x-mean(x)+mu0  # transform the data so that it is centered at the NULL
rs.mat<-c()    #rs.mat will become a resample matrix -- now it is an empty vector
xrs.mat<-c()
for(i in 1:iter){ # for loop - the loop will go around iter times
rs.mat<-cbind(rs.mat,sample(y,n,replace=TRUE)) #sampling from y cbind -- column bind -- binds the vectors together by columns
xrs.mat<-cbind(xrs.mat,sample(x,n,replace=TRUE)) #sampling from x cbind -- column bind -- binds the vectors together by columns

}

tstat<-function(z){ # The value of t when the NULL is assumed true (xbar-muo)/z/sqrt(n)
sqrt(n)*(mean(z)-mu0)/sd(z)
}

tcalc=tstat(x) # t for the data collected
ytstat=apply(rs.mat,2,tstat) # tstat of resampled y's, ytstat is a vector and will have iter values in it
xstat=apply(xrs.mat,2,mean)  # mean of resampled x's
alpha=1-conf.level # calculating alpha
ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
pvalue=ifelse(test=="two",length(ytstat[ytstat>abs(tcalc) | ytstat < -abs(tcalc)])/iter,
ifelse(test=="upper",length(ytstat[ytstat>tcalc])/iter,
length(ytstat[ytstat<xstat])/iter))

h=hist(ytstat,plot=FALSE)
mid=h$mid
if(test=="two"){
ncoll=length(mid[mid<= -abs(tcalc)])
ncolr=length(mid[mid>=  abs(tcalc)])
col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
}
if(test=="upper"){
ncolr=length(mid[mid>=  abs(tcalc)])
col=c(rep("Gray",length(mid)-ncolr),rep("Green",ncolr))
}

if(test=="lower"){
ncoll=length(mid[mid<=  -abs(tcalc)])
col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll))
}
hist(ytstat,col=col,freq=FALSE,las=1,main="",xlab=expression(T[stat]))
#segments(ci[1],0,ci[2],0,lwd=2)
pround=round(pvalue,4)
title(substitute(paste(P[value],"=",pround)))
return(list(pvalue=pvalue,tcalc=tcalc,n=n,x=x,test=test,ci=ci))
}


boot=bootpval(x=x1,mu0=24,test="two")
boot

######### End Bootstrap pvalues
## Make some fake data

set.seed(30);x=rnorm(15,mean=10,sd=7)   # fake data to understand t-tests
set.seed(40); y=rnorm(20,mean=12,sd=4)
boxplot(list(x=x,y=y)) # boxplot of a list

## Equal variances?
var.test(x,y)

## t.test
t.test(x,y, mu=0,var.equal=FALSE)


############## second set of samples 
set.seed(30);x=rnorm(15,mean=10,sd=4)   # fake data to understand t-tests
set.seed(40); y=rnorm(20,mean=12,sd=4)
boxplot(list(x=x,y=y)) # boxplot of a list

## Equal variances?
var.test(x,y)

## t.test
t.test(x,y, mu=0,var.equal=TRUE)

## Plot of the t -distr
curve(dt(x,df=15+20-2),xlim=c(-4,4))
tcut=qt(1-0.025,33)
tcut
segments(x0=c(tcut,-tcut),y0=c(0,0),x1=c(4,-4),y1=c(0,0),lwd=3)



## Bootstrap interval for a two sample test
boot2pval<-function(x1,x2,conf.level=0.95,iter=3000,mudiff=0, test="two"){
n1=length(x1)
n2=length(x2)
y1=x1-mean(x1)+mean(c(x1,x2))  # transform the data so that it is centered at the NULL
y2=x2-mean(x2)+mean(c(x1,x2))
y1rs.mat<-c()    #rs.mat will be come a resample matrix -- now it is an empty vector
x1rs.mat<-c()
y2rs.mat<-c()
x2rs.mat<-c()
for(i in 1:iter){ # for loop - the loop will go around iter times
y1rs.mat<-cbind(y1rs.mat,sample(y1,n1,replace=TRUE)) #sampling from y cbind -- column bind -- binds the vectors together by columns
y2rs.mat<-cbind(y2rs.mat,sample(y2,n2,replace=TRUE))

}
x1rs.mat<-y1rs.mat+mean(x1)-mean(c(x1,x2))
x2rs.mat<-y2rs.mat+mean(x2)-mean(c(x1,x2))

xbar1=mean(x1)
xbar2=mean(x2)
sx1sq=var(x1)
sx2sq=var(x2)

tcalc=(xbar1-xbar2-mudiff)/sqrt(sx1sq/n1+sx2sq/n2)

sy1sq=apply(y1rs.mat,2,var)
sy2sq=apply(y2rs.mat,2,var) 
y1bar=apply(y1rs.mat,2,mean)
y2bar=apply(y2rs.mat,2,mean)

tstat=(y1bar-y2bar-mudiff)/sqrt(sy1sq/n1+sy2sq/n2)


alpha=1-conf.level # calculating alpha
#ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
pvalue=ifelse(test=="two",length(tstat[tstat>abs(tcalc) | tstat < -abs(tcalc)])/iter,
ifelse(test=="upper",length(tstat[tstat>tcalc])/iter,
length(ytstat[tstat<tcalc])/iter))

h=hist(tstat,plot=FALSE)
mid=h$mid
if(test=="two"){
ncoll=length(mid[mid<= -abs(tcalc)])
ncolr=length(mid[mid>=  abs(tcalc)])
col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
}
hist(tstat,col=col,freq=FALSE)
#segments(ci[1],0,ci[2],0,lwd=2)

return(list(pvalue=pvalue))
#return(list(pvalue=pvalue,tcalc=tcalc,n=n,x=x,test=test,ci=ci))
}



set.seed(10);x=rnorm(20,mean=4,sd=2);  y=rnorm(20,mean=4,sd=2)
boot2pval(x1=x,x2=y)


### Plot alpha and beta
## Will need to use two graphs on one plot
## One sample but two different assumptions for the mean
## H0: mu=a, H1 mu=b
## We will change the cut off and see what happens to type 1, 2 and power

myab=function(a=10,b=15,sigma=5, xcut=qnorm(0.9,mean=a,sd=5/sqrt(n)),n=20){ # a for Ho, b for H1
#xcut is the cut off used to define a one tailed test
# determine the range of the x values
lmin=min(a-3*sigma/sqrt(n),b-3*sigma/sqrt(n))
rmax=max(a+3*sigma/sqrt(n),b+3*sigma/sqrt(n))

#plot the curves
curve(dnorm(x,mean=a,sd=sigma/sqrt(n)), xlim=c(lmin,rmax), main=paste("xcut=",xcut),ylab="Density")
curve(dnorm(x,mean=b,sd=sigma/sqrt(n)),add=TRUE, col="Red", lwd=2)

#vertical lines
abline(v=a)
abline(v=b)

# Text on the lines
text(a,0.5*dnorm(a,mean=a,sd=sigma/sqrt(n)),expression(H[0]),pos=1, cex=2)
text(b,0.5*dnorm(b,mean=b,sd=sigma/sqrt(n)),expression(H[1]),pos=1,cex=2)
# beta
#prob accepting H0 when it is false
# left of xcut when mean is b
bet=pnorm(xcut,mean=b,sd=sigma/sqrt(n))

#x,y coords for polygon beta
xcurve=seq(xcut,lmin,length=1000)
ycurve=dnorm(xcurve,mean=b,sd=sigma/sqrt(n))
polygon(c(xcut,xcurve,lmin),c(0,ycurve,0),col="Blue")

#x,y for polygon for alpha
xxcurve=seq(xcut,rmax,length=1000)
yycurve=dnorm(xxcurve,mean=a,sd=sigma/sqrt(n))
polygon(c(xcut,xxcurve,rmax),c(0,yycurve,0),col="Green")
}
myab(xcut=13)

layout(matrix(c(1,2,3,4),nr=2,nc=2))
myab(xcut=11)
myab(xcut=12)
myab(xcut=13)
myab(xcut=14)


###### p -values ###
## Introductory example of Null hypothesis testing
## H0:p=0.5, H1 p not =0.5
## n=10, reject region 0,1, 9,10
n=10
for(i in 1:n){
ry=rbinom(30,size=10,prob=0.5)
ry=factor(ry,levels=0:10)
ry.tab=table(ry)
barplot(ry.tab)
Sys.sleep(2)
}


y=dbinom(0:10,size=10,prob=0.5)
names(y)=0:10
barplot(y, col=c(rep("Blue",2),rep(1,7),rep("Blue",2)))
2*pbinom(1,size=10,prob=0.5)


