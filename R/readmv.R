#' @title Read all DAT data from JW
#'
#' @param dird Directory to the data folder with separator on the end
#'
#' @return A named list of all the data
#' @export
#'
#' @examples
#' \dontrun(v <- readmv())
readMV <- function(dird = 'D:\\MULTIVARIATE-OU - 2021\\DATA\\'){

  reaDAT <- function(file){
    d <- paste0(dird,file)
    f<-read.table(d, fill = TRUE, header = FALSE,skipNul = TRUE)

    f
  }

  files <- list.files(dird, pattern = "*.DAT")
  l <- stringr::str_length(files)
  newnames <- stringr::str_sub(files, 1, l - 4)

  ll <- purrr::map(files , ~ try(reaDAT(.x), silent = TRUE))
  names(ll) = newnames

  invisible(ll)

}
