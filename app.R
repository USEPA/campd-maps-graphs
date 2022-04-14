library(dotenv)
library(shiny)
library(shinybusy)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(DT)
library(data.table)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)

load_dot_env(".env")

source("./globals/global.R")
source("./modules/filter-logic.R")
source("./modules/filter-logic-single-selects.R")
source("./modules/dropdown-filter.R")
source("./modules/multiselect-filter.R")
source("./modules/line-graph.R")
source("./modules/data-table.R")
source("./allowance-app/global.R")
source("./allowance-app/page.R")
source("./facility-map-app/global.R")
source("./facility-map-app/page.R")

ui <- function(request) {
  tags$div(
    includeCSS("www/app.css"),
    uiOutput("page")
  )
}

server <- function(input, output, session) {
  
  programsAppURL <- reactiveVal({""})
  facilityMapURL <- reactiveVal({""})
  
  observe({
    programsAppURL(paste0(
      session$clientData$url_protocol,
      "//",
      session$clientData$url_hostname,
      ":",
      session$clientData$url_port,
      session$clientData$url_pathname,
      "?page=",
      "program")
    )})
  
  observe({
    facilityMapURL(paste0(
      session$clientData$url_protocol,
      "//",
      session$clientData$url_hostname,
      ":",
      session$clientData$url_port,
      session$clientData$url_pathname,
      "?page=",
      "map")
    )})
  
  observe({
    updateQueryString(facilityMapURL(), mode = "replace", session)
    
    output$page <- renderUI({
      facilityMapAppUI("id")
    })
    callModule(facilityMapAppServer, "id")
  })
  
  observe({
    currentPage <- getQueryString(session)$page
    if(!is.null(currentPage)){
      if(currentPage=="map"){
        updateQueryString(facilityMapURL(), mode = "replace", session)
        output$page <- renderUI({
          facilityMapAppUI("id")
        })
        callModule(facilityMapAppServer, "id")
      }
      
      else if(currentPage=="program"){
        updateQueryString(programsAppURL(), mode = "replace", session)
        output$page <- renderUI({
          programAppUI("id")
        })
        callModule(programAppServer, "id")
      }
    }
  })
}

#shinyApp(ui,server)

shiny::runApp(shinyApp(ui,server))
# swtich to below while deploying
#shiny::runApp(shinyApp(ui,server), host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))
