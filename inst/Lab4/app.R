# Load libraries needed
library(shiny)
#library(ggplot2)
library(purrr)
library(rootSolve)
source("Rcode.r")

spruce.df = read.csv("SPRUCE.csv")

d = spruce.df$BHDiameter



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Spruce Data Set: Piecewise Regression"),
   
   # Sidebar with a slider input for number of bins 
   
   sidebarLayout(
      sidebarPanel(
         sliderInput("xk",
                     "Choose knot:",
                     min = min(d),
                     max = max(d),
                     value = 17.44165,
                     step=0.01),
         
         sliderInput("intervalroot",
                     "choose L and U for root interval:",
                     min = min(d),
                     max = max(d),
                     value = c(15,17.55),
                     step=0.01)
                     
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("regressPlot"),
         plotOutput("R2"),
         tableOutput("root"),
         # table of data
         tableOutput("tab"),
         plotOutput("allroots")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$tab <- renderTable(spruce.df)
   

   
   output$regressPlot <- renderPlot({
     plot(spruce.df,main="Piecewise regression",pch=21,bg="black")
     
     
     sp2.df=within(spruce.df, X<-(BHDiameter-input$xk)*(BHDiameter>input$xk))  
      lmp = lm(Height ~ BHDiameter + X, data = sp2.df)
      tmp=summary(lmp) # tmp holds the summary info
      
      
      
      curve(myf(x,xk=input$xk,coef=tmp$coefficients[,"Estimate"] ),
            add=TRUE, 
            lwd=2,
            col="Blue")
      
     points(input$xk,myf(input$xk,input$xk,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="green",cex=2) 
     
     points(uroot()$root,myf(uroot()$root,uroot()$root,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="purple",cex=2) 
     
      text(input$xk,16,
           paste("R sq.=",round(tmp$r.squared,4) ))
      
   }) 
   
   uroot = reactive({
     intv = input$intervalroot
     uniroot(f=rsqdash, interval=intv, h=0.001,data=spruce.df, extendInt = "yes" )
   })
   
   urootall = reactive({
     intv = input$intervalroot
     uniroot.all(f=rsqdash, interval=intv, h=0.001,data=spruce.df )
   })
   
   output$R2 <- renderPlot({
     dsmooth = seq(min(d),max(d),length=1000)
     r2=map_dbl(dsmooth, ~rsq(.x,data=spruce.df))
     plot(dsmooth,r2,pch=21,bg="purple",
          ylab = expression(R^2 ),
          xlab = "knot",
          cex=0.5,
          main="Determination of possible knots",
          type="p",
          ylim = c(min(r2),1.1*max(r2)))
     intv = input$intervalroot
     rts=uroot()
     r2=rsq(rts$root,data=spruce.df)
     abline(v=seq(floor(min(d)), ceiling(max(d)), by=1), lwd=0.5,col="pink")
     abline(v=rts$root,h=rsq(rts$root,data=spruce.df))
     text(rts$root, r2*1.05,paste("knot is:", rts$root))
     axis(3, round(rts$root,4))
     axis(4, round(r2,4),col="Red")
    
   })
   
   
   output$root<-renderTable({
     
     intv = input$intervalroot
    rts=uroot()
    as.data.frame(rts)
     
   })
   
   
}

# Run the application 
shinyApp(ui = ui, server = server)

