#' The mode
#'
#'
#' @param x vector of data
#'
#' @return the mode of the data (most frequent)
#' @export
#'
#' @section Origin: Taken from a web page originally and then adapted \url{https://www.tutorialspoint.com/r/r_mean_median_mode.htm}
#'
#' @examples
#' \dontrun{
#' mymode(ddt$LENGTH)}
mymode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
