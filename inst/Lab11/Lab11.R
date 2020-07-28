#Lab 11
#Confidence Intervals
#set.seed(20);d=rnorm(25, mean=5, sd=0.05)
#dput(round(d,4))

d=c(5.0581, 4.9707, 5.0893, 4.9334, 4.9777, 5.0285, 4.8555, 4.9565, 
4.9769, 4.9722, 4.999, 4.9925, 4.9686, 5.0662, 4.9239, 4.9781, 
5.0485, 5.0014, 4.9957, 5.0195, 5.0118, 4.9928, 5.0361, 5.0185, 
4.9879)
t=qt(0.975,24)
ci=c()
ci[1]=mean(d)-t*sd(d)/sqrt(25)
ci[2]=mean(d)+t*sd(d)/sqrt(25)
ci

obj=t.test(d,conf.level=0.95)
names(obj)
ci



set.seed(35);sam=rnorm(30,mean=20,sd=5)
t=qt(0.975,29)
ci=c()
ci[1]=mean(sam)-t*sd(sam)/sqrt(30)
ci[2]=mean(sam)+t*sd(sam)/sqrt(30)
ci




### Fish
#set.seed(50);blue=rnorm(20,mean=20,sd=3)
#blue=round(blue,2)
#dput(blue)

blue=c(21.65, 17.48, 20.1, 21.57, 14.82, 19.17, 21.08, 18.23, 22.93, 
15.66, 20.89, 21.66, 18.5, 20.59, 18.63, 18.91, 19.53, 17.7, 
16.5, 19.03)

#set.seed(50);snapper=rnorm(15,mean=30,sd=3)
#snapper=round(snapper,2)
#dput(snapper)

snapper=c(31.65, 27.48, 30.1, 31.57, 24.82, 29.17, 31.08, 28.23, 32.93, 
25.66, 30.89, 31.66, 28.5, 30.59, 28.63)

n1=length(snapper)
n2=length(blue)
spsq=((n1-1)*var(snapper)+(n2-1)*var(blue))/(n1+n2-2)
t=qt(0.975,n1+n2-2)
ci=c()
ci[1]=mean(snapper)-mean(blue)-t*sqrt(spsq*(1/n1+1/n2)) 
ci[2]=mean(snapper)-mean(blue)+t*sqrt(spsq*(1/n1+1/n2))
ci

t.test(snapper,blue,conf.level=0.95,var.equal=TRUE)$conf.int
t.test(snapper,blue,conf.level=0.20,var.equal=T)

#Paired
#set.seed(28);exam1=rnorm(25,mean=60,sd=10)
#
#exam1=round(exam1,2)
#set.seed(38);exam2=exam1 + rnorm(25,mean=10,sd=3)
#exam2=round(exam2,2)
#dput(exam1)
#dput(exam2)
#plot(exam2-exam1 )

#Paired exams
Exam1=c(40.98, 59.36, 46.69, 41.8, 61.63, 65.31, 62.96, 60.21, 56.89, 
78.41, 53.44, 75.2, 60.54, 52.43, 41.41, 70.79, 73.55, 55.65, 
61.43, 63.84, 58.07, 53.79, 54.45, 67.18, 44.46)

Exam2=c(50.22, 66.19, 58.75, 51.88, 66.61, 70.86, 74.25, 70.23, 69.55, 
87.18, 63.62, 81.7, 70.5, 66.02, 51.35, 80.92, 85.65, 65.44, 
74.37, 75.28, 67.86, 59.92, 64.42, 73.57, 57.15)

exdif=Exam1-Exam2
t.test(exdif)$conf.int

# Birds
with(NZBIRDS, table(Extinct,Flight))




## BOOTSTRAP ###

myboot2<-function(iter=10000,x1,x2,fun="mean",alpha=0.05,...){  #Notice where the ... is repeated in the code
n1=length(x1)   #sample size
n2=length(x2)

y1=sample(x1,n1*iter,replace=TRUE)
y2=sample(x2,n2*iter,replace=TRUE)

rs1.mat=matrix(y1,nr=n1,nc=iter,byrow=TRUE)
rs2.mat=matrix(y2,nr=n2,nc=iter,byrow=TRUE)
xstat1=apply(rs1.mat,2,fun) # xstat is a vector and will have iter values in it
xstat2=apply(rs2.mat,2,fun)
xxstat=xstat1-xstat2
ci=quantile(xxstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
# A histogram follows
# The object para will contain the parameters used to make the histogram
para=hist(xxstat,freq=FALSE,las=1,
main=paste("Histogram of Bootstrap sample statistics","\n","alpha=",alpha," iter=",iter,sep=""),
...)

#mat will be a matrix that contains the data, this is done so that I can use apply()
mat1=matrix(x1,nr=n1,nc=1,byrow=TRUE)
mat2=matrix(x2,nr=n2,nc=1,byrow=TRUE)

#pte is the point estimate
#This uses whatever fun is
pte=apply(mat,2,fun)
abline(v=pte,lwd=3,col="Black")# Vertical line
#axis(1,pte,labels=TRUE)
segments(ci[1],0,ci[2],0,lwd=4)      #Make the segment for the ci
text(ci[1],0,paste("(",round(ci[1],2),sep=""),col="Red",cex=3)
text(ci[2],0,paste(round(ci[2],2),")",sep=""),col="Red",cex=3)

# plot the point estimate 1/2 way up the density
text(pte,max(para$density)/2,round(pte,2),cex=3)

return(list(ci=ci,fun=fun,x=x))# Some output to use if necessary
}

myboot(iter=10000,x=sam,fun="mean",alpha=0.05)
ci


#####################################
set.seed(45);sam=rnorm(30,mean=20,sd=5)
xsq1=qchisq(0.975,29)
xsq2=qchisq(0.025,29)
xsq1
ci=c()
ci[1]=29*var(sam)/xsq1
ci[2]=29*var(sam)/xsq2
ci[1]
ci[2]
#moment estimator for sigma^2
sum(sam^2)/30-mean(sam)^2



 myboot(iter=10000,x=sam,fun="var",alpha=0.05)



#### Find samples that give confidence intervals for the pop mean such that
#   mu is not contained within the ci

# Say we want 10 samples that meet the above criterion

outside=function(nsam=10, n=30,mn=20,std=5,...){
i=1  #initialize counters
j=0
T=c() # create a NULL vector
repeat{  # This will repeat until break()
j=j+1   # j=1,2,3 ...
sam=rnorm(n,mean=mn,sd=std)  # each loop gives a new sample
ci=t.test(sam,...)$conf.int   # create a ci from t.test
if((mn<ci[1] |mn>ci[2] )& i<=nsam) {T=cbind(T,sam);i=i+1}
#store those sams that produce cis not containing mn
if(i>nsam) {
T.test=apply(T,2,function(x) t.test(x)$conf.int)
return(list(T=T,j=j,i=i,pout=i/j*100, T.test=T.test))
# return BEFORE break
break()
}
}
}
outside(nsam=9,mn=30,n=20,std=8,T.test=T.test)

M=c()
for(k in 1:1000){

tt=outside(nsam=9,mn=30,n=50,std=8)

M=c(M,tt$pout)

}
plot(M)
hist(M)
summary(M)


set.seed(35);sam1=rnorm(25,mean=10,sd=5); set.seed(45);sam2=rnorm(34,mean=40,sd=8)
a=1-.05/2
ci=c()
ci[1]=var(sam1)/(var(sam2)*((qf(a, df1=24,df2=33))))
ci[2]=var(sam1)/var(sam2)*((qf(a, df1=33,df2=24)))
ci
var.test(sam1,sam2)