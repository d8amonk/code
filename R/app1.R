library(shiny)
ui <- fluidPage(
  # Inputs
  sliderInput(
    inputId = "num",
    label = h1("Choose a number"),
    value = 50, min = 0, max = 100),
  
  # Outputs
  plotOutput(#outputId = 'ggdist',
    "hist")
)

server <- function(input, output){
  output$hist <- renderPlot({
    title <- paste(input$num, " Random Normal Values")
    hist(rnorm(input$num), main = title, xlab = "Values")
    })
}

shinyApp(ui = ui, server = server)