myreadxl <- function(){
  dird = "D:/MATH4773-5773/DATA/Excel/"
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
  #newnames
  names(v) <- newnames
  invisible(v)
}
