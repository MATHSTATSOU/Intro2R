#' @param chains The number of MCMC chains of simulation
#'
#' @param warmup How many iterations to throw away
#' @param iter The total number of iterations
#' @param html TRUE if you want to see a browser interface
#'
#' @title Run a Bayesian model using STAN
#'
#'
#'
#' @description A basic Bayesian model estimating a population mean
#'
#' @details  Please check the STAN site for documentation
#' @return  An MCMC object
#'
#' @section STAN:
#' This function will start a STAN instance creating a STAN object which can be viewed using other packages.
#'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{ basicstan()}
basicstan<-function(chains = 3, warmup=1000, iter = 5000, html = FALSE ){
ddt <- Intro2R::ddt
basic_data <- list(y=ddt$LENGTH, N=length(ddt$LENGTH))

fit <-rstan::stan(paste0(system.file("Bayes", package="Intro2R"),"/basic.stan"),
              model_name = "basic",
              data = basic_data,
              chains = chains,
              warmup = warmup,
              iter = iter,
              pars = c("mu")
  )

if( html == FALSE){

  mu <- rstan::extract(fit)[[1]]

  g <- ggplot2::qplot(mu,main = expression(mu))

  print(g)

  print(fit,probs=c(0.025,0.5,0.975))
}

else{

  afit <- shinystan::as.shinystan(fit)
  shinystan::launch_shinystan(afit)

}

}
