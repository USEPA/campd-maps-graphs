
# clear quotes below while deploying

'library_path <- paste("Library Path: ", Sys.getenv(c("LD_LIBRARY_PATH")))
print(paste("LD_LIBRARY_PATH: ", library_path))

lib_dir <- "/home/vcap/deps/0/r/lib"
local_lib_dir <- "lib"

if(dir.exists(lib_dir))
{
  # Get the list of libs
  lib_tars <- list.files(local_lib_dir)
  lib_tars <- paste(local_lib_dir, lib_tars, sep="/")
  
  print(paste("Local libs: ", lib_tars))
  print(paste("Working directory: ", list.files(getwd())))
  
  # Copy the files to the lib_dir
  for(i in 1:length(lib_tars)) {untar(lib_tars[i], exdir = lib_dir)}
  
  Sys.setenv(PROJ_LIB=lib_dir)
}

print(list.files(lib_dir))'

library(dotenv)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinybusy)
library(shinyjs)
library(shinyWidgets)
library(shinydisconnect)
library(sf)
library(leaflet)
library(leaflet.extras)
library(plotly)
library(DT)
library(shinyjs)
library(httr)
library(htmltools)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(data.table)
library(zoo)

load_dot_env(".env")

#globals
source("./globals/global-static.R")
source("./globals/global-reactive.R")
source("./globals/global-load.R")
source("./facility-map-app/global.R")
source("./allowance-app/global.R")

#modules
source("./modules/filter-logic.R")
source("./modules/filter-logic-placeholder-single-selects.R")
source("./modules/filter-logic-select-all-single-selects.R")
source("./modules/dropdown-filter.R")
source("./modules/multiselect-filter.R")
source("./modules/display-table.R")
source("./modules/search.R")
source("./modules/line-graph.R")
source("./modules/data-table.R")

#pages
source("./allowance-app/page.R")
source("./facility-map-app/page.R")

ui <- function(request) {
  fluidPage(
    tags$head(tags$link(rel="shortcut icon", href=paste0(gitRawBase,"/www/favicon.ico"))),
    tags$html(class="CAMPDRShiny"),
    tags$html(lang="en"),
    class="main-page",
    includeCSS("www/app.css"),
    includeScript('www/script.js'),
    # adding load spinner
    add_busy_spinner(
      spin = "fading-circle",
      color = "#112446",
      timeout = 100,
      position = c("full-page"),
      height = "50px",
      width = "50px"
    ),
    # disconnect
    disconnectMessage(
      text = "Your session timed out! Please reload the application.",
      refresh = "Reload now",
      background = "white",
      colour = "#000000",
      overlayColour = "#f0f0f0",
      overlayOpacity = 0.3,
      refreshColour = "#1a4480"
    ),
    uiOutput("page")
  )
}

server <- function(input, output, session) {
  
  global_vars()
  
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
      "program-insights")
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
      "facility-map")
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
      if(currentPage=="facility-map"){
        updateQueryString(facilityMapURL(), mode = "replace", session)
        output$page <- renderUI({
          facilityMapAppUI("id")
        })
        callModule(facilityMapAppServer, "id")
      }
      
      else if(currentPage=="program-insights"){
        updateQueryString(programsAppURL(), mode = "replace", session)
        output$page <- renderUI({
          programAppUI("id")
        })
        callModule(programAppServer, "id")
      }
    }
  })
}

shiny::runApp(shinyApp(ui,server))
# switch to below while deploying
#shiny::runApp(shinyApp(ui,server), host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))
