#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram


shinyServer(function(input, output) {

    x    <- faithful[, 2]

response <- reactive({
    y <-rnorm(1000, mean(x)*input$mult, sd(x))
    list(y=y)
})



    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

    output$distPlot2 <- renderPlot({
y <- response()$y
        # generate bins based on input$bins from ui.R
        bins <- seq(min(y), max(y), length.out = input$bins2 + 1)

        # draw the histogram with the specified number of bins
        hist(y, breaks = bins, col = 'darkgray', border = 'white')

    })

    output$box <- renderPlot({
        y <- response()$y
    out<-myttest(x=x,y=y,paired = FALSE,alpha=0.05)

    plot(out)
    })


})
