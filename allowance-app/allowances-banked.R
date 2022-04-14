### UI

allowancesBankedUI <- function(id) {
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
      titlePanel("Allowance Bank"),
      fluidRow(
        column(10, textOutput(ns("allowanceBankText"))),
        column(2, actionButton(ns("stopanimation"), "Stop Animation"))),
      sidebarLayout(
      sidebarPanel(h3("Filters"), 
                   radioButtons(ns("levelOfAnalysis"), "Level of Analysis",
                                c("Program"="p", 
                                  "State"="s"),
                                selected = "p"
                                ),
                   conditionalPanel(
                     condition = "input.levelOfAnalysis == 'p'", ns = ns,
                     dropdownSelectUI(ns("programInput"), 
                                      label= singleLabelConversion$label[singleLabelConversion$columnName == "programDescription"],
                                      placeholder_label= "a program",
                                      choices= sort(uniquePrograms))
                     ),
                   conditionalPanel(
                     condition = "input.levelOfAnalysis == 's'", ns = ns,
                     columnFilterSetUI(ns("filterset"), 
                                       allowanceBankFilterIndicesState
                                       )
                              
                     ),
                   actionButton(ns("previewButton"), "Preview Data")
                   ),
      mainPanel(
        uiOutput(ns("programPlots")),
        )
      ),
      dataTableUI(ns("programTable"))
      )
    )
}


allowancesBankedServer <- function(input, output, session) {
  output$allowanceBankText <- renderText({
    "Use this tool to..."
  })
  observeEvent(input$stopanimation, {
    stop_gif()
  })
  
  selectedDataSet <- reactive({applicableAllowCompTable})
  filteredData <- reactiveVal(applicableAllowCompTable)
  choices <- reactiveValues(programsSelected=NULL,
                            statesSelected=NULL)
    
  filterSetReturn <- callModule(columnFilterSet, "filterset", 
                                df = selectedDataSet, 
                                allowanceBankFilterIndicesState,
                                programIsSingleSelect=TRUE)
    
  filterSingleReturn <- callModule(dropdownSelectServer,"programInput",
                                   df = selectedDataSet,
                                   columnToFilter="programDescription")
    
  observeEvent(input$levelOfAnalysis,{
    output$plots <- renderUI({
      tagList()
    })
    output$summaryTable <- renderDataTable( data.frame() )
  })
  
  observeEvent(c(filterSetReturn$selections, filterSingleReturn$selections),{
    if (is.null(input$levelOfAnalysis) | length(input$levelOfAnalysis)==0)
      return()
    if (input$levelOfAnalysis == "p"){
      choices$programsSelected <- filterSingleReturn$selections
      filteredData(filterSingleReturn$data)
    }
    else{
      choices$programsSelected <- filterSetReturn$selections[["programDescription"]]
      choices$statesSelected <- filterSetReturn$selections[["stateName"]]
      filteredData(filterSetReturn$data)
    }
  })
    
  observeEvent(input$previewButton,{
    programsSelected <- currentCompliancePrograms$programCode[currentCompliancePrograms$programDescription %in% 
                                              choices$programsSelected]
    statesSelected <- states$stateCode[states$stateName %in% choices$statesSelected]
      
    if (length(programsSelected) > 0){
      complianceYears <- currentCompliancePrograms[currentCompliancePrograms$programCode %in% 
                                                     programsSelected,]$complianceYears
      minimumYear <- min(unlist(complianceYears))
      maximumYear <- max(unlist(complianceYears))
    }
    if (input$levelOfAnalysis == "s"){
      if (length(programsSelected) == 0 || length(statesSelected) == 0 ){
        showModal(modalDialog(
          title = "Input missing",
          "Please make a state and program selection.",
          easyClose = TRUE
        ))
        return()
      }
      
      facilityComplianceData <- get_allow_comp_data(
        complianceYears = seq(minimumYear,maximumYear),
        programs = programsSelected, states = statesSelected)
        
      output$programPlots <- renderUI({
        tagList(
          tabsetPanel(type = "tabs",
                      tabPanel("Allocated", 
                                lineGraphUI(session$ns("allocatedPlot"))
                      ),
                      tabPanel("Deducted", 
                                lineGraphUI(session$ns("deductedPlot"))
                      ),
                      
                      tabPanel("Banked", 
                               lineGraphUI(session$ns("carriedPlot"))
                      )
          )
        )
      })
        
      aggregatedComplianceData <- getStatePlotData(facilityComplianceData,
                                                   c("allocated", 
                                                     "totalAllowancesDeducted",
                                                     "carriedOver"))
      
      callModule(lineGraphSever, "allocatedPlot", 
                 df=aggregatedComplianceData, xVals='year', yVals='allocated',
                 group='stateName', graphTitle="Allowances Allocated", 
                 xtitle="Years", ytitle="Number of Allowances", 
                 hoverText=paste('Allowances: %{y}','<br>Year: %{x}'),
                 xAxisIsYears=TRUE)
        
      callModule(lineGraphSever, "deductedPlot", 
                 df=aggregatedComplianceData, xVals='year', yVals='totalAllowancesDeducted', 
                 group='stateName', graphTitle="Allowances Deducted", 
                 xtitle="Years", ytitle="Number of Allowances", 
                 hoverText=paste('Allowances: %{y}','<br>Year: %{x}'),
                 xAxisIsYears=TRUE)
        
      callModule(lineGraphSever, "carriedPlot", 
                 df=aggregatedComplianceData, xVals='year', yVals='carriedOver',
                 group='stateName', graphTitle="Allowances Banked", 
                 xtitle="Years", ytitle="Number of Allowances", 
                 hoverText=paste('Allowances: %{y}','<br>Year: %{x}'),
                 xAxisIsYears=TRUE)
        
      tableData <- data.frame(stateName=aggregatedComplianceData$stateName, 
                              programCode=aggregatedComplianceData$programCode,
                              year=aggregatedComplianceData$year, 
                              allocated=aggregatedComplianceData$allocated, 
                              totalAllowancesDeducted=aggregatedComplianceData$totalAllowancesDeducted, 
                              carriedOver=aggregatedComplianceData$carriedOver)
      
      names(tableData) <- tableLabelConversion$label[match(names(tableData), 
                                                           tableLabelConversion$columnName)]
      
    }
    else{
      if (length(programsSelected) == 0){
        showModal(modalDialog(
          title = "Input missing",
          "Please make a program selection.",
          easyClose = TRUE
        ))
          return()
      }
      
      facilityComplianceData <- get_allow_comp_data(
        complianceYears = seq(minimumYear,maximumYear),
        programs = programsSelected)
      
      names(facilityComplianceData)[names(facilityComplianceData) == 
                                      'programCodeInfo'] <- 'programCode'
      
      output$programPlots <- renderUI({
        tagList(
          lineGraphUI(session$ns("programPlot"))
          )
        })
      
      complianceColumns <- c("allocated", 
                             "totalAllowancesDeducted",
                             "carriedOver")
      rowDisplayColumnNames <- tableLabelConversion$label[match(complianceColumns,tableLabelConversion$columnName)]
        
      tableData <- getProgramTableData(facilityComplianceData, 
                                              complianceColumns)
      
      names(tableData) <- tableLabelConversion$label[match(names(tableData), 
                                                           tableLabelConversion$columnName)]
      
      programPlotData <- getPlotDataByColumns(tableData,rowDisplayColumnNames)
      
      callModule(lineGraphSever, "programPlot", 
                 df=programPlotData, xVals='Year', yVals='Value',
                 group='Category', graphTitle=paste0(choices$programsSelected,
                                                     " Allowance Information"),
                 xtitle="Years", ytitle="Number of Allowances", 
                 hoverText=paste('Allowances: %{y}','<br>Year: %{x}'),
                 xAxisIsYears=TRUE)
    }
    
    callModule(dataTableSever,"programTable", 
               "Summary Data Table", tableData)
  })
  
  # RENAME FUNCTIONS BELOW!
  
  getStatePlotData <- function(dataframe, columns){
    
    indices <- match(columns
                     ,names(dataframe))
      
    aggregatedData <- aggregate(dataframe[,indices],
                                list(dataframe$programCode,
                                     dataframe$stateCode,
                                     dataframe$year),
                                sum)
    colnames(aggregatedData)[1:3] <- c("programCode",
                                       "stateCode",
                                       "year")
    aggregatedData = merge(x=aggregatedData,
                           y=states[,c("stateCode","stateName")],
                           by.x="stateCode")
    
    aggregatedData
      
  }
    
  getProgramTableData <- function(dataframe, columns){
      
    indices <- match(columns
                     ,names(dataframe))
    
    aggregatedData <- aggregate(dataframe[,indices],
                                list(dataframe$programCode,
                                     dataframe$year),
                                sum)
    
    colnames(aggregatedData)[1:2] <- c("programCode","year")
    
    aggregatedComplianceData = merge(x=aggregatedData,
                                     y=currentCompliancePrograms[,c("programCode",
                                                                    "programDescription")],
                                     by.x="programCode")
    
    aggregatedComplianceData
    
  }
  
  getPlotDataByColumns <- function(dataframe, columns){
    
    plotData <- bind_rows(lapply(columns, function(col){
      columnsToBind <- select(dataframe, -all_of(columns))
      categoryValue <- bind_rows(lapply(1:nrow(dataframe), function(i){
        data <- data.frame(col,dataframe[[col]][i])
        colnames(data) <- c("Category", "Value")
        data
      }))
      cbind(columnsToBind,categoryValue)
    }))
    
    plotData
  }
}


