#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

myout <- function(x) ifelse(abs(x) > 3, "OUTLIER",
                            ifelse(abs(x)>=2 & abs(x)<=3, "Poss. Out.", "MAIN"))

ddt<-ddt%>%mutate(zL = scale(LENGTH), zW = scale(WEIGHT), zM = scale(MILE), zD = scale(DDT), zLo = factor(myout(zL)), zWo = factor(myout(zW)),zMo = factor(myout(zM)), zDo = factor(myout(zD)) )
