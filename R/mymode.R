#' @title Function to find the mode
#'
#'
#' @param v vector of data
#'
#' @return the mode of the data (most frequent)
#' @export
#'
#' @examples
#' \dontrun{
#' mymode(ddt$LENGTH)}
mymode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
