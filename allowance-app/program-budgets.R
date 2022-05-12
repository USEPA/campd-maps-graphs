### UI

programBudgetsUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(HTML("<title>Program Budgets</title>")), 
      add_busy_spinner(
        spin = "half-circle",
        color = "#112446",
        timeout = 100,
        position = c("top-right"),
        onstart = TRUE,
        margins = c(10, 10),
        height = "50px",
        width = "50px"
      ),
      h1("Program Budgets"),
      fluidRow(column(12, 
                      tags$ul(
                        tags$li("Use this tool to map annual state allowance budgets
                                allocated against annual emissions of facilities within
                                a particular state for each allowance trading program."),
                        tags$li('Data is mapped as an Allowance-Emission ratio. 
                                For example, a state with a ratio of 1.5 in 
                                2018 was budgeted 1.5 allowances for every ton 
                                of emissions they emitted in 2018.'),
                        tags$li('States with higher ratios (<insert color scheme>) 
                                have reduced emissions below their budgets and 
                                use less allowances to meet compliance.'),
                        tags$li('Explore Allowance-Emission ratios and 
                                breakdowns by hovering over individual states.'),
                        tags$li('CSAPR Programs include a mechanism called 
                                "Assurance Provisions" which ensure emission 
                                reduction within each individual state. These 
                                provisions add a variable limit to each state 
                                budget.'),
                        tags$li('For more information visit the',
                        tags$a(href="https://www.epa.gov/csapr/csapr-assurance-provision", 
                                       "CSAPR Assurance Provisions page",
                               target="_blank"),
                                ".")
                      )),
               #column(2, actionButton(ns("stopanimation"), "Stop Animation"))
      ),
      sidebarLayout(
        sidebarPanel(h2("Filters"),
                     rowFilterPlaceholderSingleSelectSetUI(ns("filterset"), programBudgetFilterIndicesState),
                     
                     div(class="clear-preview-btns",
                         actionButton(ns("clearFilters"), "Clear Filters"),
                         actionButton(ns("previewButton"), "Preview Data")
                     )
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
  
  observeEvent(input$stopanimation, {
    stop_gif()
  })
  
  budgetDataFrame <- reactive({state_budgets})
  
  filtered_data <- callModule(columnFilterPlaceholderSingleSelectSet, 
                              "filterset", 
                              df = budgetDataFrame, 
                              programBudgetFilterIndicesState,
                              reactive(c(input$clearFilters)))
  
  choices <- reactiveValues(selectedProgram=NULL,
                            selectedYear=NULL,
                            selectedAssuranceInclude=NULL)
  
  observeEvent(input$previewButton,{
    if (filtered_data$selections[["year"]]=="" | 
        filtered_data$selections[["programCode"]]=="" | 
        filtered_data$selections[["assuranceFlag"]]==""){
      showModal(modalDialog(
        title = "Input missing",
        "Please make a program, year, and assurance level selection.",
        easyClose = TRUE
      ))
      return()
    }
    
    programCodeSelect <- filtered_data$selections[["programCode"]]
    yearSelect <- as.integer(filtered_data$selections[["year"]])
    assuranceIncluded <- filtered_data$selections[["assuranceFlag"]]
    
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
    
    if (assuranceIncluded == "yes"){
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
                                         allocated/ceiling(so2Mass),
                                         allocated/ceiling(noxMass)),
               excessAllow = if_else((programCode %in% so2AnnualPrograms), 
                                     allocated - ceiling(so2Mass), 
                                     allocated - ceiling(noxMass)), 
               percentExcess = excessAllow/allocated*100)
    }
    
    if (programCodeSelect %in% so2AnnualPrograms){
      budgetTableData <- budgetData %>% select(year, programCode, stateName, 
                                               so2Mass, allocated,
                                               variabilityLimit, assuranceLevel,
                                               allowEmissRatio, excessAllow,
                                               percentExcess)
      hoverText <- paste("<strong>",budgetData$stateName,"</strong>", "<br>", 
                         "Allocations:", budgetData$allocated, "<br>", 
                         "Assurance Level:", budgetData$assuranceLevel, "<br>",
                         "SO2 Emissions:", budgetData$so2Mass, "<br>",
                         "Excess Allowances:", budgetData$excessAllow, "<br>", 
                         "Allowance to Emission Ratio:", budgetData$allowEmissRatio, "<br>")
    }
    else{
      budgetTableData <- budgetData %>% select(year, programCode, stateName, 
                                               noxMass, allocated,
                                               variabilityLimit, assuranceLevel,
                                               allowEmissRatio, excessAllow,
                                               percentExcess)
      hoverText <- paste(budgetData$stateName, "<br>",
                         "Allocations:", budgetData$allocated, "<br>", 
                         "Assurance Level:", budgetData$assuranceLevel, "<br>",
                         "NOx Emissions:", budgetData$noxMass, "<br>",
                         "Excess Allowances:", budgetData$excessAllow, "<br>", 
                         "Allowance to Emission Ratio:", budgetData$allowEmissRatio, "<br>")
    }
    
    output$programBudgetPlot<- renderPlotly({
      
      # specify some map projection/options
      g <- list(
        showframe = T,
        showcoastlines = F,
        scope = "usa",
        fitbounds = "locations",
        subunitcolor = "#79d2a4",
        projection = list(type = "albers usa")
      )
      
      fig <- plot_geo(budgetData, locationmode = "USA-states", reversescale = T)
      fig <- fig %>% add_trace(
        z = ~allowEmissRatio, 
        text = hoverText, 
        hoverinfo = 'text',
        locations = ~stateCode,
        color = ~allowEmissRatio, colors = "PuBu"
      )
      fig <- fig %>% colorbar(title = "Allocations to<br>Emissions Ratio")
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
    
    names(budgetTableData) <- tableLabelConversion$label[match(names(budgetTableData), 
                                                         tableLabelConversion$columnName)]
    filename <- paste0("program-budgets-",programCodeSelect,
                       "-",as.character(yearSelect),"-assurance-included-",assuranceIncluded,
                       ".csv")
    
    callModule(dataTableSever,"budgetsTable", 
               "Summary Data Table", unique(budgetTableData), 
               filename)
    
  })
}



