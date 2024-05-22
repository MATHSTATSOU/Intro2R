#' @title Read all xl files
#'
#' @description This function will read a collection of xls files into R's workplace
#'
#' @details This will greatly reduce the amount of work needed to perform analyses since the reading of files can be time consuming.
#'
#' @param dird this is the directory structure as a string
#' @importFrom utils choose.dir
#'
#' @return a named list of the files read
#' @export
#'
#' @examples
#' \dontrun{
#' v<-myreadxl()
#' }
myreadxl <- function(dird = "D:/MATH4773-5773/DATA/Excel/"){
  if( !dir.exists(dird) & .Platform$OS.type == "windows") {
    message("Use window to find excel directory")
   dird <-  choose.dir(caption = "Choose the excel directory!")
   dird <- paste0(dird, "\\")
  } else({
    if( !dir.exists(dird) & .Platform$OS.type != "windows"){
      stop("You need the correct directory to the Excel folder (must end in directory separator)")
    }
  })

  #library(readxl)

  files = list.files(dird)
  #files

  myconvert = function(xl) {
    if(stringr::str_ends(xl, "XLS") | stringr::str_ends(xl, "xls")){
      v=try(readxl::read_xls(paste0(dird, xl)), silent = TRUE)
    }
    else{
      v = NA
    }
    v
  }

  v  = purrr::map(files, ~myconvert(.x))
  l <- stringr::str_length(files)
  #l
  newnames <- stringr::str_sub(files,1,l-4)
  #new names
  names(v) <- newnames
  v$dird <- dird
  message(paste0("This is the Excel path:\n", dird))
  invisible(v)
}
