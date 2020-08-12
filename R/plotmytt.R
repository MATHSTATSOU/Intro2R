#' @title  Method for plotting class mytt
#'
#' @description Makes a boxplot using an object of class mytt
#'
#' @details The function \code{myttest()} produces a list of class mytt, which has three components. These are sourced from the object and used in \code{plot.mytt()}
#'
#' @param x object class mytt
#'
#' @param ... extra options to be sent to method
#'
#' @return  A boxplot of the two samples using \code{ggplot()}
#'
#' @importFrom ggplot2 ggplot aes ggtitle geom_boxplot
#'
#' @importFrom graphics plot
#'
#'
#' @importFrom stats var.test t.test
#'
#' @rdname plot.mytt
#'
#' @export
#'
#' @examples
#' l<-myttest(x=rnorm(30), y=rnorm(40,0.5));plot(l)
plot.mytt <- function(x, ...){
  df<-x$df
  g<-ggplot(df, aes(x=v,y=data)) + geom_boxplot(aes(fill=v))
  g<-g + ggtitle(paste(
    "P value =",
    round(x$ttest$p.value,4),
    "Paired = ",
    x$paired
    )
  )
  print(g)
}
