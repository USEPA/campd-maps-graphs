library(shiny)
library(dotenv)

load_dot_env(".env")

appfold <- "01-app"

shiny::runApp(appfold)
# swtich to below while deploying
#shiny::runApp(appfold, host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))
