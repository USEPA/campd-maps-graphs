
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
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)

load_dot_env(".env")

source("./globals/global-static.R")
source("./globals/global-load.R")
source("./modules/filter-logic.R")
source("./modules/filter-logic-placeholder-single-selects.R")
source("./modules/filter-logic-select-all-single-selects.R")
source("./modules/dropdown-filter.R")
source("./modules/multiselect-filter.R")
source("./modules/display-table.R")
source("./modules/search.R")
source("./modules/line-graph.R")
source("./modules/data-table.R")
source("./allowance-app/page.R")
source("./facility-map-app/page.R")

source("./facility-map-app/global.R")
source("./allowance-app/global.R")


#timeoutSeconds <- 5

#inactivity <- sprintf("function idleTimer() {
#  var t = setTimeout(logout, %s);
#  window.onmousemove = resetTimer; // catches mouse movements
#  window.onmousedown = resetTimer; // catches mouse movements
#  window.onclick = resetTimer;     // catches mouse clicks
#  window.onscroll = resetTimer;    // catches scrolling
#  window.onkeypress = resetTimer;  //catches keyboard actions
#  
#  function logout() {
#    Shiny.setInputValue('timeOut', '%ss')
#  }
#  
#  function resetTimer() {
#    clearTimeout(t);
#    t = setTimeout(logout, %s);  // time is in milliseconds (1000 is 1 second)
#  }
#}
#idleTimer();", timeoutSeconds*1000, timeoutSeconds, timeoutSeconds*1000)

'disconnectMessage(
      text = "Your session timed out, reload the application.",
      refresh = "Reload now",
      background = "#f89f43",
      colour = "white",
      overlayColour = "grey",
      overlayOpacity = 0.3,
      refreshColour = "brown"
    ),'

ui <- function(request) {
  fluidPage(
    tags$html(class="CAMPDRShiny"),
    tags$html(lang="en"),
    class="main-page",
    includeCSS("www/app.css"),
    # disconnect
    uiOutput("page")
  )
}

server <- function(input, output, session) {
  
  #session$allowReconnect(TRUE)
  
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
