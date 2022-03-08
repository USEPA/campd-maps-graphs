multiSelectUI <- function(id, label, choices) {
  ns <- NS(id)
  tagList(
    selectizeInput(ns("multiselection"), 
                label, choices, 
                multiple = TRUE)
  )
}

multiSelectServer <- function(input, output, session, 
                              incoming_input, colCondition, 
                              #reactive_choices, # think about this more
                              reactive_data) {
  
  # think about this more
  #observeEvent(reactive_data(),{
  #  updateSelectizeInput(session, "multiselection", choices=reactive_choices())
  #})
  
  x <- reactiveValues(data=NULL)
  
  selection <- reactive({
    #validate(need(input$multiselection, FALSE))
    input$multiselection
  })
  
  observeEvent(input$multiselection, {
    x$data <- reactive_data()[reactive_data()[[colCondition]] %in% selection(),]
  })
  
  observeEvent(incoming_input$clearButton, {
    updateSelectizeInput(session, "multiselection", selected = "")
    x$data <- reactive_data()
  })
  
  return(reactive({x$data}))
  
}