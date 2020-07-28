mynewt=function(x0,delta=0.001,f,fdash){
d=1000
i=0
x=c()
y=c()
x[1]=x0
y[1]=f(x[1])
while(d > delta & i<10000){
i=i+1
x[i+1]=x[i]-f(x[i])/fdash(x[i])
y[i+1]=f(x[i+1])
d=abs(y[i])
}
#windows()
curve(f(x),xlim=range(c(range(x),-range(x))),xaxt="n", main="Newton-Raphson Algorithm")
points(x,y,col="Red",pch=19,cex=1.5)
axis(1,x,round(x,2),las=2)
abline(h=0,col="Red")

segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Pink")

list(x=x,y=y)
}




mynewt2=function(x0,delta=0.001,f, h=0.0001){
  
  fdash = function(x) (f(x+h)-f(x))/h
  d=1000
  i=0
  x=c()
  y=c()
  x[1]=x0
  y[1]=f(x[1])
  while(d > delta & i<10000){
    i=i+1
    x[i+1]=x[i]-f(x[i])/fdash(x[i])
    y[i+1]=f(x[i+1])
    d=abs(y[i])
  }
  #windows()
  curve(f(x),xlim=range(c(range(x),-range(x))),xaxt="n", main="Newton-Raphson Algorithm, estimated fdash")
  points(x,y,col="Red",pch=19,cex=1.5)
  axis(1,x,round(x,2),las=2)
  abline(h=0,col="Red")
  
  segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
  segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Pink")
  
  list(x=x,y=y)
}

layout(matrix(1:2,nr=1))
mynewt(x0=10,delta=0.000001,f=function(x) x^2-4,fdash=function(x) 2*x )
mynewt2(x0=10,delta=0.000001,f=function(x) x^2-4)

myNRML=function(x0,delta=0.001,llik,xrange,parameter="param"){
f=function(x) (llik(x+delta)-llik(x))/delta
fdash=function(x) (f(x+delta)-f(x))/delta
d=1000
i=0
x=c()
y=c()
x[1]=x0
y[1]=f(x[1])
while(d > delta & i<100){
i=i+1
x[i+1]=x[i]-f(x[i])/fdash(x[i])
y[i+1]=f(x[i+1])
d=abs(y[i+1])
}
layout(matrix(1:2,nr=1,nc=2,byrow=TRUE),width=c(1,2))
curve(llik(x), xlim=xrange,xlab=parameter,ylab="log Lik",main="Log Lik")
curve(f(x),xlim=xrange,xaxt="n", xlab=parameter,ylab="derivative",main=  "Newton-Raphson Algorithm \n on the derivative")
points(x,y,col="Red",pch=19,cex=1.5)
axis(1,x,round(x,2),las=2)
abline(h=0,col="Red")

segments(x[1:(i-1)],y[1:(i-1)],x[2:i],rep(0,i-1),col="Blue",lwd=2)
segments(x[2:i],rep(0,i-1),x[2:i],y[2:i],lwd=0.5,col="Green")

list(x=x,y=y)
}


mynewt(x0=10,delta=0.000001,f=function(x) x^2-4,fdash=function(x) 2*x )

myNRML(x0=0.99,delta=0.000001,llik=function(x) log(dbinom(12,size=20,prob=x)*dbinom(10,size=25,prob=x)),xrange=c(0.01,0.99),parameter="p" )
myNRML(x0=1,delta=0.000001,llik=function(x) log(dpois(12,x)*dpois(10,x)),xrange=c(0,20),parameter="lambda" )