#' @title Binomial Simulation
#'
#' @description Shows how to simulate a Binomial
#'
#' @details More details about the function
#'
#' @param iter Number of iterations
#' @param n Number of Bernoulli trials
#' @param p Probability of a success
#'
#' @importFrom grDevices  rainbow
#' @importFrom graphics  barplot
#'
#' @return Barplot and table of relative frequencies
#' @export
#'
#' @examples
#' mybin() # Use default values
mybin=function(iter=100,n=10, p=0.5){
  # make a matrix to hold the samples
  #initially filled with NA's
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  #Make a vector to hold the number of successes in each trial
  succ=vector(mode="numeric", length=iter)
  for( i in 1:iter){
    #Fill each column with a new sample
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    #Calculate a statistic from the sample (this case it is the sum)
    succ[i]=sum(sam.mat[,i])
  }
  #Make a table of successes
  succ.tab=table(factor(succ,levels=0:n))
  #Make a barplot of the proportions
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
