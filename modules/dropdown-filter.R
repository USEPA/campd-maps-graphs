dropdownSelectUI <- function(id, label, placeholder_label, choices) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("dropselection"), 
                label, choices, 
                options = list(placeholder = paste0('--Select ',placeholder_label,'--'),
                               onInitialize = I('function() { this.setValue(""); }')),
                multiple = FALSE)
  )
}

dropdownSelectServer <- function(input, output, session, df, 
                                 columnToFilter
                                 ) {
  
  toReturn <- reactiveValues(selections=NULL, data=df)
  
  selection <- reactive({
    #validate(need(input$dropselection, FALSE))
    input$dropselection
  })
  
  observeEvent(input$dropselection, {
    if (!isTruthy(selection())){
      toReturn$selections <- NULL
      toReturn$data <- df()
    }
    else if (length(selection()) > 0) {
      toReturn$selections <- selection()
      toReturn$data <- df()[df()[[columnToFilter]] == selection(),]
    }
    else{toReturn$data <- df()}
  })
  
  return(toReturn)
  
}