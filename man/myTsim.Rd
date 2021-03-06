% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mytsim.R
\name{myTsim}
\alias{myTsim}
\title{T simulation}
\usage{
myTsim(
  n1 = 10,
  sigma1 = 3,
  mean1 = 5,
  iter = 1000,
  ymax = 0.4,
  x = 2,
  y = 0.3,
  ...
)
}
\arguments{
\item{n1}{Sample size}

\item{sigma1}{Population standard deviation}

\item{mean1}{Population mean}

\item{iter}{Number of iterations}

\item{ymax}{Yaxis maximum value (for plots)}

\item{x}{The x co-ord of the T annotation formula}

\item{y}{The y co-ord of the same}

\item{...}{More parameters to send to the histogram}
}
\value{
A plot and list of summary stats
}
\description{
This carries out a simulation from a  Normal
}
\details{
This demonstrates what a sampling distribution is and gives some details of how an r simulation can be made. The function is RMD ready.
}
\examples{
myTsim()
}
