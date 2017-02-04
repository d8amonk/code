library(shiny)
ui <- fluidPage("Hello World!")

# 3 rules of 
# save output as *output obj
# use render* obj to make the output
# make the output obj dependent on input obj
server <- function(input, output){}

shinyApp(ui = ui, server = server)