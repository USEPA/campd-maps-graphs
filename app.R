library(shiny)
library(dotenv)

load_dot_env(".env")

appfold <- "01-app"

#runApp(appfold)
runApp(appfold, host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))