# Mini Lab

library(shiny)
library(ggplot2)
library(Intro2R)
library(prodlim)
library(crayon)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  # Application title
  titlePanel("Leave one out correlation statistic"),

  # Sidebar with a  slider
  sidebarLayout(
    sidebarPanel(
      sliderInput("digits",
                  "Number of digits:",
                  min = 1,
                  max = 8,
                  value = 4),

      sliderInput("size",
                "Plot point SIZE:",
                min = 0,
                max = 10,
                value = 1,
                step = 0.2)),

    # Show a ggplot
    mainPanel(
      plotOutput("plot", click = "plot_click"),
      tableOutput("data")
    )
  )
)

# ui <- fluidPage(
#   plotOutput("plot", click = "plot_click"),
#   tableOutput("data")
# )

server <- function(input, output, session) {
  output$plot <- renderPlot({
    ggplot(ddt, aes(x = WEIGHT, y = LENGTH)) +
      geom_smooth(method = "lm", formula = y ~ x ) +
      geom_point(aes(shape = RIVER, col = SPECIES), size = input$size) +
      ggtitle(substitute(r == corr, list(corr = format(cor(ddt$WEIGHT, ddt$LENGTH), digits = input$digits))))
  }, res = 96)

  output$data <- renderTable({

    req(input$plot_click)
    df <- nearPoints(ddt,
                     input$plot_click,
                     threshold = 8,
                     maxpoints = 1)

    row <- row.match(df,ddt)
    ddtmr <- ddt[-row,c(4,5)]

    r <- cor(ddtmr, method = "pearson")[1,2]

    r <- format(r, digits = input$digits)



    ylm <- lm(LENGTH ~ WEIGHT, ddtmr )

    coefs = data.frame(b0 = ylm$coefficients[1], b1 = ylm$coefficients[2])
    r <- data.frame(correlation = r)
    cbind(r,df, coefs)
  },bordered = TRUE, digits = 4
)

}

# Run the application
shinyApp(ui = ui, server = server)
