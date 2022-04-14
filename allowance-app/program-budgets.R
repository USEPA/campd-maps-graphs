### UI

programBudgetsUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      add_busy_spinner(
        spin = "half-circle",
        color = "#112446",
        timeout = 100,
        position = c("top-left"),
        onstart = TRUE,
        margins = c(10, 10),
        height = "50px",
        width = "50px"
      ),
      titlePanel("Program Budgets"),
      fluidRow(
        column(10, textOutput(ns("programBugetsText"))),
        column(2, actionButton(ns("stopanimation"), "Stop Animation"))),
      sidebarLayout(
        sidebarPanel(h3("Filters"),
                     rowFilterSingleSelectSetUI(ns("filterset"), programBudgetFilterIndicesState),
                     selectizeInput(ns("assuranceLevel"), 
                                    "Include Assurance Level (required)", choices=c("Yes","No"), 
                                    options = list(placeholder = paste0('--Select y/n--'),
                                                   onInitialize = I('function() { this.setValue(""); }')),
                                    multiple = FALSE),
                     actionButton(ns("previewButton"), "Preview Data")
                     ),
        mainPanel(
          plotlyOutput(ns("programBudgetPlot"))
          )
        ),
      dataTableUI(ns("budgetsTable"))
      )
    )
}

programBudgetsServer <- function(input, output, session) {
  output$programBugetsText <- renderText({
    "Use this tool to..."
  })
  observeEvent(input$stopanimation, {
    stop_gif()
  })
  
  budgetDataFrame <- reactive({state_budgets})
  
  filtered_data <- callModule(columnFilterSingleSelectSet, 
                              "filterset", 
                              df = budgetDataFrame, 
                              programBudgetFilterIndicesState,
                              isRequired=TRUE)
  
  choices <- reactiveValues(selectedProgram=NULL,
                            selectedYear=NULL,
                            selectedAssuranceInclude=NULL)
  
  observeEvent(filtered_data$selections,{
    if (as.integer(filtered_data$selections[["year"]])<2017){
      updateSelectizeInput(session, "assuranceLevel",
                           choices = c("No"),
                           selected = "No")
    }
    else {
      currentSelection <- input$assuranceLevel
      updateSelectizeInput(session, "assuranceLevel",
                           choices = c("Yes","No"),
                           selected = currentSelection)
    }
  },ignoreInit = TRUE)
  
  observeEvent(input$previewButton,{
    if (input$assuranceLevel == ""){
      showModal(modalDialog(
        title = "Input missing",
        "Please make an assurance level selection.",
        easyClose = TRUE
      ))
      return()
    }
    
    programCodeSelect <- currentCompliancePrograms$programCode[currentCompliancePrograms$programDescription == filtered_data$selections[["programDescription"]]]
    yearSelect <- as.integer(filtered_data$selections[["year"]])
    assuranceIncluded <- input$assuranceLevel
    
    if (programCodeSelect %in% c(noxAnnualPrograms, so2AnnualPrograms)){
      emissionsData <- get_annual_emiss_data(emissionYears = c(yearSelect), programs = c(programCodeSelect))
    }
    else{
      emissionsData <- get_ozone_emiss_data(emissionYears = c(yearSelect), programs = c(programCodeSelect))
    }
    
    aggregateEmissions <- aggregate(cbind(so2Mass,noxMass) ~ stateCode + year, data = emissionsData, FUN = sum, na.rm = TRUE)
    aggregateEmissions$programCode <- programCodeSelect
    budgetEmissions <- merge(x=aggregateEmissions,
                             y=state_budgets,
                             by=c("stateCode", "year", "programCode"))
    
    if (assuranceIncluded == "Yes"){
      budgetData <- budgetEmissions %>% 
        mutate(allowEmissRatio = if_else((programCode %in% so2AnnualPrograms), 
                                         assuranceLevel/ceiling(so2Mass),
                                         assuranceLevel/ceiling(noxMass)),
               excessAllow = if_else((programCode %in% so2AnnualPrograms), 
                                     assuranceLevel - ceiling(so2Mass), 
                                     assuranceLevel - ceiling(noxMass)), 
               percentExcess = excessAllow/assuranceLevel*100)
    }
    
    else{
      budgetData <- budgetEmissions %>% 
        mutate(allowEmissRatio = if_else((programCode %in% so2AnnualPrograms), 
                                         allocations/ceiling(so2Mass),
                                         allocations/ceiling(noxMass)),
               excessAllow = if_else((programCode %in% so2AnnualPrograms), 
                                     allocations - ceiling(so2Mass), 
                                     allocations - ceiling(noxMass)), 
               percentExcess = excessAllow/allocations*100)
    }
    
    output$programBudgetPlot<- renderPlotly({
      budgetData$hover <- with(budgetData, paste(stateName, "<br>", 
                                                 "Allocations", allocations, "<br>", 
                                                 "Assurance Level", assuranceLevel, "<br>",
                                                 "Excess Allowances", excessAllow, "<br>"))
      
      # specify some map projection/options
      g <- list(
        showframe = T,
        showcoastlines = F,
        scope = "usa",
        fitbounds = "locations",
        subunitcolor = "#3399FF",
        projection = list(type = "albers usa")
      )
      
      fig <- plot_geo(budgetData, locationmode = "USA-states", reversescale = T)
      fig <- fig %>% add_trace(
        z = ~allowEmissRatio, text = ~hover, locations = ~stateCode,
        color = ~allowEmissRatio, colors = "PuBu"
      )
      fig <- fig %>% colorbar(title = "Allocations to Emissions Ratio<br>(Allocations/Emissions)")
      fig <- fig %>% layout(
        title = "Title<br>(Hover for breakdown)",
        geo = g,
        autosize=F,
        margin= list(
          l= 0,
          r= 0,
          b= 0,
          t= 0,
          pad= 4,
          autoexpand=T
        )
      )
      
      fig
    })
    
    callModule(dataTableSever,"budgetsTable", 
               "Summary Data Table", budgetData)
    
  })
}



