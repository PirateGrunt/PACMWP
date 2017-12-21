library(shiny)
library(shinythemes)

source("main.R")

ui <- fluidPage(
  theme = shinytheme("superhero")
  , navbarPage(
      id = "navMain"
    , title = "PACMWP"
    , tabMain
  )
)

server <- function(input, output, session) {

  eval(main_output)

}

shinyApp(ui, server)
