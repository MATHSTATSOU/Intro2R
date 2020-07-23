layout(matrix(1:4, nr=2,nc=2))

layout.show(4)

curve(dnorm(x, mean=10,sd=4),xlim=c(-10,30))

curve(dnorm(x, mean=10,sd=8),xlim=c(-10,30))

curve(dnorm(x, mean=5,sd=4),xlim=c(-10,30))

curve(dnorm(x, mean=10,sd=2),xlim=c(-10,30))


### Draw a normal curve
curve(dnorm(x, mean=10,sd=4),xlim=c(-10,30))

# Find the area between x=10 and 25

# x values corresponding to the x - cords of points on the curve
xcurve=seq(10,25,length=1000)

# Y values corresponding t0 the x values
ycurve=dnorm(xcurve,mean=10,sd=4)

# Fill in the polygon with the given vertices
polygon(c(10,xcurve,25),c(0,ycurve,0),col="Red")

# Put in the text with the appropriate area

# Area
prob=pnorm(25,mean=10,sd=4)-pnorm(10,mean=10,sd=4)
prob=round(prob,4)

# Click to paste the text onto the graph
text(locator(1), paste("Area = ", prob, sep=""))

###############  Gamma ################
curve(dgamma(x,shape=1,scale=1),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
ylab="Gamma density", main="Beta=1")
text(locator(1),paste("alpha=",1))
curve(dgamma(x,shape=3,scale=1),xlim=c(0,10),ylim=c(0,1),add=TRUE,lwd=2)
text(locator(1),paste("alpha=",3))
curve(dgamma(x,shape=5,scale=1),xlim=c(0,10),ylim=c(0,1),add=TRUE, col="Blue",lwd=2)
text(locator(1),paste("alpha=",5))

################### Chi -sq ######### alpha = df/2, beta=2
curve(dchisq(x,df=2),xlim=c(0,10),ylim=c(0,1),col="Red",lwd=2,
ylab="Chisq density", main="df=2,5")
text(locator(1),paste("df=",2))
curve(dchisq(x,df=5),xlim=c(0,10),ylim=c(0,1),col="Blue",lwd=2,
ylab="Chisq density",add=TRUE)

## Beta
curve(dbeta(x,shape1=2,shape2=3),xlim=c(0,1))

#P( 0.2<=Y<=0.6)
