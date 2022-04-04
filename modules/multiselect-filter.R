multiSelectUI <- function(id, label, choices) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("multiselection"), 
                label, choices, 
                multiple = TRUE, options = list(
                  'plugins' = list('remove_button'),
                  'create' = TRUE,
                  'persist' = FALSE))
  )
}

multiSelectServer <- function(input, output, session, 
                              df, columnToFilter) {
  
  toReturn <- reactiveValues(data=df,selections=list())
  
  selection <- reactive({
    #validate(need(input$multiselection, FALSE))
    input$multiselection
  })
  
  observe({
    if (!isTruthy(selection())){
      toReturn$selections[[columnToFilter]] <- NULL
      toReturn$data <- df()
    }
    if (length(selection()) > 0) {
      toReturn$selections[[columnToFilter]] <- selection()
      toReturn$data <- df()[df()[[columnToFilter]] %in% selection(),]
    }
    else{toReturn$data <- df()}
  })
  
  return(toReturn)
  
}