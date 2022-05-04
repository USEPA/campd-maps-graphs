
dataTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tableOutput"))
  )
}

dataTableSever <- function(input, output, session, 
                           tableTitle, tableData) {
  
  output$tableOutput <- renderUI({
    tagList(
      h2(tableTitle),
      dataTableOutput(session$ns("tableDisplay"))
    )
  })
  
  
  output$tableDisplay <- renderDataTable( 
    tableData,
    rownames = FALSE,
    extensions = 'Buttons',
    options = list(
                   pageLength = 10,
                   scrollX = TRUE,
                   dom = 'Bftsp',
                   buttons = list(list(extend = "copy", 
                                       text = "Copy Table"),
                                  list(extend = "csv", 
                                       text = "Download CSV"))
                     
                     #c('copy', 'csv', 'excel')
                   )
  )
  
}

