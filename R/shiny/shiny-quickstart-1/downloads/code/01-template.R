library(shiny)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      fluidPage(
        br("Testing!"),
        br(),
        br(),
        br(),
        br(),
        sliderInput(inputId = "num", 
          label = "Choose a number!", 
          value = 500, min = 1, max = 1000)
      )
    ),
    mainPanel(
      h3("This is a test"),
      fluidPage(
        fluidRow(
          column("One", width = 6)
          ),
        fluidRow(column("Two.1", width = 2), column("Two.2", width = 2)),
        fluidRow("3"),
        fluidRow("4")
      )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)