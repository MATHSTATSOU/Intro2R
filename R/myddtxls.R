#' Reading raw data
#'
#' @return ddt data
#' @export
#'
#' @examples
#' ddt2 <- myddtxls()
myddtxls <- function(){
  address <- system.file("extdata", "DDT.XLS",package = "Intro2R")
  readxl::read_xls(address)
}
