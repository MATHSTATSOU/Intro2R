#' @title Links to files in directory
#'
#' @description This will place a link in the RMD document to files in the Intro2R package
#'
#' @details  This makes the construction of links easier
#'
#' @param pack this is the package name, Intro2R is default
#' @param rootf this is the folder in the root of the package where files are located
#'
#' @return rmd code for setting up link
#' @export
#'
#' @examples
#' \dontrun{
#' `r rmdfiles(rootf="Lab2")`
#' }
rmdfiles <- function(rootf, pack = "Intro2R"){
  dirdl <- system.file(rootf,package = pack)
  files <- list.files(dirdl)
  purrr::map(files, ~rmdfile(.x,.x,rootf))
}
# rmdfile <- function(link,file,rootf){
#   dirdl <- system.file(rootf,package = "Intro2R")
#   dir2f <- paste0(dirdl,"/",file)
#
#   paste0("[",link,"]","(",dir2f, ")")
#
# }
