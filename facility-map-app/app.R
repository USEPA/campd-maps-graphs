# app
source('./page.R')

ui <- function(){
  facilityMapAppUI("id_page")
}

server <- function(input, output, session) {
  callModule(facilityMapAppServer, "id_page")
}

shinyApp(ui, server)