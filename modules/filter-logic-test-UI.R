library(shiny)
library(shinydashboard)
source("filter-logic.R")

# Run after loading API info (key and url) 

column_indexes <- match(c("stateCode","controlCode","fuelTypeCode","unitTypeCode","year"),names(fac_data))

ui <- dashboardPage(
  dashboardHeader(title = "Facility Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Table" , tabname = "my_table", icon = icon("table"),
               menuSubItem("sub menu",
                           tabName = "subMenu")),
      menuItem("Next Widget", tabName = "Other"))
  ),
  dashboardBody(
    selectInput("dataset", "Program", unique(fac_data$programCode), selected = unique(fac_data$programCode)[1]),
    tags$div(style = "padding-left: 18px;",
      columnFilterSetUI("filterset", column_indexes)
    ),
    DT::dataTableOutput("table")
  )
)

server <- function(input, output, session) {
  
  selected_data <- reactive({
    fac_data[fac_data$programCode == input$dataset,]
  })
  
  filtered_data <- callModule(columnFilterSet, "filterset", df = selected_data, column_indexes)
  
  output$table <- DT::renderDataTable( filtered_data(),
    options = list(scrollX = TRUE))
}

shinyApp(ui, server)