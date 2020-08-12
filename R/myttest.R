#' @title A constructor function for t tests
#'
#' @description   This is an example of what can be done with S3 OOP
#'
#' @details The constructor function makes a list that can be worked and further processed with an appropriate method(s)
#'
#' @param x vector of data
#' @param y vector of data
#' @param paired TRUE or FALSE, FALSE by default
#' @param alpha alpha level between 0,1
#'
#' @importFrom utils data
#'
#' @return list containing the data, and t test object
#' @export
#'
#' @keywords ttest t.test testing NHST
#'
#'
#'
#' @seealso \code{\link{t.test}} for more information about t tests
#'
#' @examples
#' myttest(x=rnorm(30,10,12), y=rnorm(40, 7, 10))
myttest<-function(x,y, paired=FALSE, alpha=0.05){

  data <- vector(mode = "numeric", length = length(x) + length(y))
   v <- vector(mode = "list", length = length(data))

  if(paired=="FALSE"){ # not paired then ...
    vt=var.test(x,y)

  if(vt$p.value>alpha){
    tt<-t.test(x,y,var.equal = TRUE, conf.level=1-alpha)
  }
  else{
    tt<-t.test(x,y,var.equal = FALSE, conf.level=1-alpha)
  }
  }
  else{ # paired so check lengths
    stopifnot(length(x)==length(y))
    tt<-t.test(x,y,paired=TRUE,conf.level=1-alpha)
  }
  data <- c(x,y)
  v <- rep(c("x","y"), c(length(x),length(y))) # Creation of qual var
  df = data.frame("data" = data, "v" = v)
  lst=list(ttest=tt, df=df, paired = paired)
 class(lst)<-"mytt"# New class
 lst
}

#' @title Another function
#'
#' @details You may add as many functions as you wish in this file and document them in Roxygen
#'
#' @param x vector
#'
#' @return plot
#' @export
#'
#' @examples
#' x<-1:10;mynewf(x)
mynewf<-function(x){
  plot(x)
}
