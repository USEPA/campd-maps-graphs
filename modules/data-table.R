
dataTableUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("tableOutput"))
  )
}

dataTableSever <- function(input, output, session, 
                           tableTitle, tableData, filename) {
  
  output$tableOutput <- renderUI({
    if (is.null(tableData)){return()}
    tagList(
      div(class="DT-header-row",
          div(class="DT-header-column",h2(tableTitle)),
          div(class="DT-header-column",downloadButton(session$ns("download_data"),"Download Data"))
          ),
      dataTableOutput(session$ns("tableDisplay"))
    )
  })
  
  
  output$tableDisplay <- renderDataTable( 
    tableData,
    rownames = FALSE,
    #paging = TRUE,
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = '<fprtil>'
      #dom = '<"top"fp>rt<"bottom"il>',
      #buttons = list(list(extend = "copy", 
      #                   text = "Copy Table"),
      #              list(extend = "csv", 
      #                   text = "Download CSV"))
      
      #c('copy', 'csv', 'excel')
    )
  )
  
  output$download_data <- downloadHandler(
    filename =  function() { filename },
    content = function(file) {
      write.csv(tableData, file)
    }
  )
  
}

