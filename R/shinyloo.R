#' @title Leave One Out stats
#'
#'
#'
#' @description Widgets, plotting areas and layouts should be mastered
#'
#' @details  Please note the basic structure of the server input output
#' @return  Plots
#'
#' @section shiny:
#' The shiny web server will run once this function is invoked and will open a web browser. You should learn how this is implemented.
#'
#' The web server can be studied \url{https://shiny.rstudio.com/tutorial/}
#'
#' @section Shiny server:
#' When building the server function in the app make sure your remember the three steps:
#'
#' \enumerate{
#' \item Must populate the output list
#' \item Must use Render functions
#' \item Must use the input list
#' }
#'
#' @section Shiny reactivity:
#'
#' The main engine that drives \emph{shiny server} is the concept of \emph{reactivity} this is what makes shiny apps responsive.
#' The concept is a very important one and you must understand it completely by visiting a video and then reading an article.
#' \enumerate{
#' \item Please visit \href{https://vimeo.com/rstudioinc/review/131218530/212d8a5a7a/#t=47m27s}{Reactivity}
#' \item Read \href{https://shiny.rstudio.com/articles/reactivity-overview.html}{this informative article}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{ shinyloo()}
shinyloo <- function(){
  shiny::runApp(system.file("shinyloo",
                            package="Intro2R"),
                launch.browser = TRUE)
}
