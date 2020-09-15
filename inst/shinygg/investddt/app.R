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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DDT data set"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            varSelectInput("condvar",
                           "Categorical fill Variable:",
                           ddt[,c("SPECIES","RIVER")],
                           selected =  "SPECIES"),
            varSelectInput("var",
                           "Quantitative Variable:",
                           ddt[,c("MILE","LENGTH", "WEIGHT","DDT")],
                          selected =  "LENGTH"
                          ),
            # varSelectInput("Zout",
            #                "Extreme Quant. values?:",
            #                ddt[,c("zLo","zWo", "zDo")],
            #                selected =  "zLo"),

        ),


        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("histPlot"),
           plotOutput("dotPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$histPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      g <- ggplot(ddt, aes(x = !!input$var, fill = !!input$condvar ))
      g <- g + geom_histogram(bins = input$bins)


        # draw the histogram with the specified number of bins
        g
    })
    output$dotPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
ifelse(input$var == "LENGTH", ZZ <- "zLo",
        ifelse(input$var == "WEIGHT", ZZ <-"zWo",
                ifelse(input$var == "DDT", ZZ <- "zDo", ZZ <-"zMo")))
      # sw <- switch(input["var"][1], LENGTH = "zLo", WEIGHT = "zWo", MILE = "zMo", DDT = "zDo")

      #sw<-noquote(sw)

        g <- ggplot(ddt, aes(x = !!input$var))
        g <- g + geom_dotplot(aes(color = !!input$condvar,stroke = 5, fill = ddt[,ZZ]))
        g <- g + geom_density(aes(y = ..count..))


        # draw the histogram with the specified number of bins
        g
    })

}

# Run the application
shinyApp(ui = ui, server = server)
