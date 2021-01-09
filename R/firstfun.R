#' @title  Very first function
#'
#' @description  This instructs the student on the basics of function making
#'
#' @details  Welcome to STATS at OU -- we will be learning R from the ground up. RAR man!
#'
#' @seealso \code{\link{plot}}
#'
#' @param x This is the name of a vector
#'
#' @param ... extra arguments which will be sent to the corresponding ... in this case the plot function
#'
#' @return A list of x and y data in vector form and a matrix of the same as well as  a plot of the data
#'
#'
#'
#'
#'
#' @export
#'
#' @section Making Functions:
#'
#' Learn every part of this simple but important function.
#'
#' \enumerate{
#' \item Where does the name of the function go?
#' \item What does the ellipsis do?
#' \item How long is the object \code{y} in relation to \code{x}?
#' \item Where do you find more information about \code{plot}?
#' \item What is a list?
#' \item what does the function \code{return} do?
#' }
#'
#' @examples
#' ####### some examples ####
#' data <- 1:40
#' myfirstfun(x = data, pch=21, bg="Blue", cex = 2.4) # x data and extra options to send to plot
myfirstfun <- function(x, ...) { # key function "function(), name of function , ... sends extra arguments to plot
  y <- x^2 #object on the right x^2,  name on the left "y"
  graphics::plot(x,y,main = "My first R function", ...) # plot function -- use ?plot to learn more about

  mat <-matrix(c(x,y), nrow = length(x),byrow = FALSE)
  return(list(x=x,y=y, mat = mat)) # return will stop execution of code and release argument to the command line

}




#' @title DDT data set.
#'
#' @description  A dataset containing measurements on fish caught on the Tennessee River
#'
#' @details This is a standard data set from the course MATH 4753 taken from the data sets provided by the text book.
#'
#' @format A data frame with 144 rows and 6 variables:
#' \describe{
#'   \item{DDT}{ppm DDT in the flesh of fish}
#'   \item{LENGTH}{Length of fish in cm}
#'   \item{MILE}{Miles up river where fish is caught}
#'   \item{RIVER}{River name}
#'   \item{SPECIES}{Fish species}
#'   \item{WEIGHT}{Weight of fish}
#' }
#'
#' @source \url{https://www.routledge.com/Statistics-for-Engineering-and-the-Sciences-Sixth-Edition/Mendenhall-Sincich/p/book/9781498728850}
"ddt"



#' @title Flights data set.
#'
#' @description  A dataset containing measurements on flights
#'
#' @details This is a standard data set used in tutorials showing how to use the data.table package.
#'
#' @format A data frame with 253316  rows and 11 variables:
#' \describe{
#'   \item{year}{year}
#'   \item{month}{month}
#'   \item{day}{day}
#'   \item{dep_delay}{departure delay}
#'   \item{arr_delay}{arrival delay}
#'   \item{carrier}{carrier}
#'   \item{origin}{origin}
#'   \item{dest}{destination}
#'   \item{air_time}{time in the air}
#'   \item{distance}{distance travelled}
#'   \item{hour}{hour}
#' }
#'
#' @source \url{https://raw.githubusercontent.com/Rdatatable/data.table/master/vignettes/flights14.csv}
"flights"
