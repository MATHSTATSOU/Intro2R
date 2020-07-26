#' @title  T simulation
#'
#' @description This carries out a simulation from a sample Normal
#'
#' @details This demonstrates what a sampling distribution is and gives some details of how an r siulation can be made. The function is RMD ready.
#'
#' @param n1 Sample size
#' @param sigma1 Population standard deviation
#' @param mean1  Population mean
#' @param iter  Number of iterations
#' @param ymax Yaxis maximum value (for plots)
#' @param x The x co-ord of the T annotation formula
#' @param y The y co-ord of the same
#' @param ... More parameters to send to the histogram
#'
#' @return A plot and list of summary stats
#' @export
#'
#' @examples
#' myTsim()
myTsim<-function(n1=10,sigma1=3,mean1=5,iter=1000,ymax=0.1,x=2,y=0.3,...){    # adjust ymax to make graph fit
  y1=rnorm(n1*iter,mean=mean1,sd=sigma1)# generate iter samples of size n1

  data1.mat=matrix(y1,nrow=n1,ncol=iter,byrow=TRUE) # Each column is a sample size n1

  sd1=apply(data1.mat,2,sd) # sd
  ybar=apply(data1.mat,2,mean)  # mean

  w=(ybar-mean1)/(sd1/sqrt(n1))      #T stat

  hist(w,freq=FALSE, ylim=c(0,ymax), # Histogram with annotation
       main=substitute(paste("Sample size = ",n[1]," = ",n1," statistic = ",T," iterations= ",iter)),
       xlab=expression(paste(T, "Statistic",sep=" ")), las=1,...)
  lines(density(w),col="Blue",lwd=3) # add a density plot
  curve(dt(x,n1-1),add=TRUE,col="Red",lty=2,lwd=3) # add a theoretical curve
  title=expression(T==frac((bar(y)-mu),s/sqrt(n1))) #mathematical annotation -see ?plotmath
  legend(x,y,c("Simulated","Theoretical"),col=c("Blue","Red"),lwd=4,lty=1:2,bty="n",title=title) # Legend #
  invisible(list(w=w,summary=summary(w),sd=sd(w),fun="T")) # some output to use if needed
}
