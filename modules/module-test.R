# Testing Modules

library(shiny)

source("dropdown-filter.R")
source("multiselect-filter.R")

df <- iris

ui <- fluidPage(
  mainPanel(
    h1("Compliance Info"),
    dropdownSelectUI("specSelect", label="Chose a Species", 
                     placeholder_label="Species", choices=unique(df$Species)),
    multiSelectUI("pedWidSelection", label="Select Petal Width", choices=unique(df$Petal.Width)),
    actionButton("clearButton", "Clear All"),
    fluidRow( h3("Table"),
              dataTableOutput("table2"),
              style = "width:990px; overflow-y: scroll;overflow-x: scroll;"
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactiveVal(NULL)
  
  spec_selected <- callModule(dropdownSelectServer,
                                 "specSelect",
                                 incoming_input=input,
                                 placeholder_label="Species",
                                 colCondition="Species"
  )
  
  # Note that year is stored in global
  filtered_data_by_spec <- reactive({
    if(is.null(spec_selected())){
      filtered_data(NULL)
    }
    else if(spec_selected()!=""){
      filtered_data(df[df$Species == spec_selected(),])
      filtered_data()
    }
    else{
      filtered_data(NULL)
    }
  })
  
  multi_return_data <- callModule(multiSelectServer,
                                  "pedWidSelection",
                                  incoming_input=input,
                                  colCondition="Petal.Width",
                                  reactive_data=filtered_data_by_spec)
  
  output$table2 <- renderDataTable({
    multi_return_data()
  })
  
}

shinyApp(ui = ui, server = server)