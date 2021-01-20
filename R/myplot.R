#' @title A function for plotting a quadratic
#' @description A basic function
#'
#'
#' @param x The diameter
#' @param q a quadratic lm object
#'
#' @return Response ie estimated Height
#' @export
#'
#' @examples
#' \dontrun{quad.lm<-lm(y~x,data=df); myplot(x=15, q = quad.lm)}
myplot=function(x,q){
  q$coef[1] +q$coef[2]*x  + q$coef[3]*x^2
}
