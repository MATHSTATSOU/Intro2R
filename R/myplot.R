#' @title A function for plotting a quadratic
#'
#' @param x The diameter
#'
#' @return Response ie estimated Height
#' @export
#'
#' @examples
#' \dontrun{quad.lm<-lm(y~x,data=df); myplot(x=15)}
myplot=function(x){
  quad.lm$coef[1] +quad.lm$coef[2]*x  + quad.lm$coef[3]*x^2
}
