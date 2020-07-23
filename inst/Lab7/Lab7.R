#This is a model R function that you can alter for other statistics
# Copy this function twice and alter the two copies to make sampling distributions from the T distribution
mychisim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,...){    # adjust ymax to make graph fit
y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1

data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1

ssq1=apply(data1.mat,2,var) # ssq1 is s squared

w=(n1-1)*ssq1/sigma1^2      #chi-sq stat

hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",chi^2)),
xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
lines(density(w),col="Blue",lwd=3) # add a density plot
curve(dchisq(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
title=expression(chi^2==frac((n[1]-1)*s^2,sigma^2)) #mathematical annotation -see ?plotmath
legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #
return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq")) # some output to use if needed
}
windows()
chisq=mychisim(iter=10000,ymax=0.15)





#### Two pop sampling
mychisim2<-function(n1=10,n2=14,sigma1=3,sigma2=3,mean1=5,mean2=10,iter=1000,ymax=0.07,...){    # adjust ymax to make graph fit
y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1
y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1
data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
ssq1=apply(data1.mat,2,var) # ssq1 is s squared
ssq2=apply(data2.mat,2,var)
spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2) # pooled s squared
w=(n1+n2-2)*spsq/(sigma1^2)#sigma1=sigma2,  Chi square stat
hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",chi^2)),
xlab=expression(paste(chi^2, "Statistic",sep=" ")), las=1)
lines(density(w),col="Blue",lwd=3) # add a density plot
curve(dchisq(x,n1+n2-2),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
title=expression(chi^2==frac((n[1]+n[2]-2)*S[p]^2,sigma^2)) #mathematical annotation -see ?plotmath
legend(locator(1),c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #
return(list(w=w,summary=summary(w),sd=sd(w),fun="Chi-sq")) # some output to use if needed
}
windows()
mychisim2(iter=10000)

myTsim2<-function(n1=10,n2=14,sigma1=3,sigma2=3,mean1=5,mean2=10,iter=1000,ymax=0.5,...){
y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1
y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1
data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
ssq1=apply(data1.mat,2,var) # ssq1 is s squared
ybar1= apply(data1.mat,2,mean)
ssq2=apply(data2.mat,2,var)
ybar2=apply(data2.mat,2,mean)
spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2) # pooled s squared
w=((ybar1-ybar2)-(mean1-mean2))/sqrt(spsq*(1/n1+1/n2))#sigma1=sigma2,  Chi square stat
hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",T)),
xlab=paste(" T Statistic",sep=""), las=1)
lines(density(w),col="Blue",lwd=3) # add a density plot
curve(dt(x,n1+n2-2),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
title=expression(T==frac((bar(Y)[1]-bar(Y)[2])-(mu[1]-mu[2]),S[p]*sqrt(frac(1,n[1])+frac(1,n[2])))) #mathematical annotation -see ?plotmath
legend(2,0.2,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)# Legend #
return(list(w=w,summary=summary(w),sdw=sd(w),fun="T")) # some output to use if needed
}
myTsim2(iter=10000)


myFsim2<-function(n1=10,n2=14,sigma1=3,sigma2=2,mean1=5,mean2=10,iter=1000,ymax=0.9,...){
y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1
y2=rnorm(n2*iter,mean=mean2,sd=sigma2)
data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1
data2.mat=matrix(y2,nrow=n2,ncol=iter,byrow=TRUE)
ssq1=apply(data1.mat,2,var) # ssq1 is s squared
ssq2=apply(data2.mat,2,var)
#spsq=((n1-1)*ssq1 + (n2-1)*ssq2)/(n1+n2-2) # pooled s squared
w=ssq1*sigma2^2/(ssq2*sigma1^2) #
hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
main=substitute(paste("Sample size = ",n[1]+n[2]," = ",n1+n2," statistic = ",F)),
xlab=paste("F Statistic",sep=""), las=1)
lines(density(w),col="Blue",lwd=3) # add a density plot
curve(df(x,n1-1,n2-1),xlim=c(0,6),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
title=expression(F==frac(s[1]^2,s[2]^2)*frac(sigma[2]^2,sigma[1]^2)) #mathematical annotation -see ?plotmath
legend(6,0.5,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title)# Legend #
return(list(w=w,summary=summary(w),sd=sd(w),fun="F")) # some output to use if needed
}
myFsim2(iter=10000)
