library(dotenv)
library(shiny)
library(shinyjs)
library(shinybusy)
library(shinydashboard)
#library(leaflet)
library(plotly)
library(httr)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(zoo)
#library(leaflet.extras) 

load_dot_env(".env")

source("./globals/global.R")
source("./modules/filter-logic.R")
source("./modules/multiselect-filter.R")
source("./modules/line-graph.R")

appfold <- "allowance-app"

shiny::runApp(appfold)
# swtich to below while deploying
#shiny::runApp(appfold, host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))
