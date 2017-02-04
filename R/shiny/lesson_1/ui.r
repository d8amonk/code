library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  a("http://www.google.com"),
  br(),
  hr(),
  img(src = "https://blog.prepscholar.com/hubfs/body_testinprogress.gif"),
  code("
       this is a code() #chunk
     "),
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))