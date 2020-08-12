#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

data = read.csv("INVQUAD.csv")
head(data)
tt=t.test(INV.QUAD ~ PLANT,mu=0, data=data)
library(shiny)
library(s20x)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Example 8.13 pg 396"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("alpha",
                     "Error rate: as a percentage:",
                     min = 0,
                     max = 20,
                     value =5 ),
         sliderInput("nr",
                     "Number of rows in the data:",
                     min = 1,
                     max = 100,
                     value =5 )
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel("Boxplot", plotOutput("bxplt")),
          tabPanel("QQNORM", plotOutput("qq")),
          tabPanel("(1-alpha)100% CI", tableOutput("summary")),
          tabPanel("Table", tableOutput("table"))
        )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  
  linear <- reactive({
    obj= t.test(INV.QUAD ~ PLANT,conf.level=1-input$alpha/100,mu=0, data=data)
    obj
  })
  
   output$bxplt <- renderPlot({
     
    
      # generate bins based on input$bins from ui.R
      plot(INV.QUAD ~ PLANT,
           main=paste("Pvalue = ", round(linear()$p.value,4 )),
           col = c("Blue","Green"), 
           data)
      
      
   })
   
   output$qq <-renderPlot({
     
     with(data,normcheck(lm(INV.QUAD ~ PLANT),shapiro.wilk=TRUE))
   })
   
   output$summary <-renderText({
     linear()$conf.int
   })
   
   output$table <- renderTable({
     head(data, input$nr)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

