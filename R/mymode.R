#' @title Function to find the mode
#'
#'
#' @param x vector of data
#'
#' @return the mode of the data (most frequent)
#' @export
#'
#' @examples
#' \dontrun{
#' mymode(ddt$LENGTH)}
mymode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
