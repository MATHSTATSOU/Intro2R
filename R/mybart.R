#' Hypothesis testing Introduction
#'
#' @param n Number of trials
#' @param r Number of Blue upper/lwer tail bars
#'
#' @return Barplot
#' @export
#'
#' @examples
#' mybart()
mybart <- function(n = 10, r = 2){

  py <- dbinom(x = 0:n,
               size = n,
               prob = 1/2)
  names(py) = 0:n
  barplot(py,
          col = rep(c("blue", "green", "blue"),c(r,(n+1)-2*r,r)),
          xlab = "Successes",
          main = "RAR: H_0: p = 1/2")
}
