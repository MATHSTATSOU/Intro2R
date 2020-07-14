#' @title Link and file location
#'
#' @description This will place a link in the RMD document to a file in the Intro2R package
#'
#' @details  This makes the construction of links easier
#'
#' @param link the name of the link to be shown in the rmd docyment
#' @param file the file name in folder "rootf"
#' @param rootf this is the folder in the root of the package where files are located
#'
#' @return rmd code for setting up link
#' @export
#'
#' @examples
#' \dontrun{
#' `r rmdfile("Lab2 exemplar code","Lab2.R", "Lab2")`
#' }
rmdfile <- function(link,file,rootf){
  dirdl <- system.file(rootf,package = "Intro2R")
  dir2f <- paste0(dirdl,"/",file)

  paste0("[",link,"]","(",dir2f, ")")

}
