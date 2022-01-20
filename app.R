appfold <- "01-app"

#shiny::runApp(appfold)
shiny::runApp(appfold, host="0.0.0.0", port=strtoi(Sys.getenv("PORT")))
