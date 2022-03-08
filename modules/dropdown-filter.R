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

dropdownSelectServer <- function(input, output, session, incoming_input,
                                 placeholder_label, colCondition) {
  
  x <- reactiveValues(selected=NULL)
  
  selection <- reactive({
    validate(need(input$dropselection, FALSE))
    input$dropselection
  })
  
  observeEvent(input$dropselection, {
    x$selected <- input$dropselection
  })
  
  observeEvent(incoming_input$clearButton, {
    updateSelectizeInput(session, "dropselection", options = list(placeholder = paste0('--Select ',placeholder_label,'--'),
                                                            onInitialize = I('function() { this.setValue(""); }')))
    x$selected <- NULL
  })
  
  return(reactive({x$selected}))
  
}