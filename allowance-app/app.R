# app
source('./page.R')

ui <- function(){
  programAppUI("id_page")
}

server <- function(input, output, session) {
  callModule(programAppServer, "id_page")
}

shinyApp(ui, server)