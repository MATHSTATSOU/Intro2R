#' Print method for class Rttest
#'
#' @param x data from constructor function
#' @param ... table variables
#' @author Jordan Daugherty (2022)
#' @importFrom knitr kable
#' @importFrom kableExtra kable_styling add_header_above
#' @importFrom magrittr '%>%'
#'
#' @return a table with the data and the alpha value, confidence intervals and p-value.
#' @export print.Rttest
#'
#' @export
#'
#' @examples
#' \dontrun{print(x)}
print.Rttest <- function(x, ...) {
  df<-x$df
  data <- data.frame(alpha=x$alpha,confidence1=x$confidence[1],confidence2=x$confidence[2],pvalue=x$pvalue)
  print(knitr::kable(x = df,digits=3,col.names = NULL) %>%
          kable_styling(full_width = FALSE, bootstrap_options = "condensed",position="float_left") %>%
          add_header_above(c("Data"=2)))

  print(knitr::kable(x = data,digits=3,col.names = NULL) %>%
          kable_styling(full_width = FALSE, bootstrap_options = "condensed",position="float_left") %>%
          add_header_above(c("Alpha"=1,"Confidence"=2,"P-Value"=1)))
}
