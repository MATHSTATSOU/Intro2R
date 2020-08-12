#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
#library(shinyexample)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data, and Normal"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins: waiting time",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot")
        )
    ),


    sidebarLayout(
        sidebarPanel(
            sliderInput("bins2",
                        "Number of bins: Normal",
                        min = 1,
                        max = 50,
                        value = 30),


        sliderInput("mult",
                    "multiplier for mean: Normal",
                    min = 1,
                    max = 1.05,
                    value = 1,
                    step=0.001)
    ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot2"),
            plotOutput("box")
        )
    )
))
