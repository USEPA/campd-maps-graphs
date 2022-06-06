### UI

allowancesBankedUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      tags$head(HTML("<title>Allowance Trends</title>")), 
      h1("Allowance Trends"),
      fluidRow(
        column(12, p("This tool uses annual compliance data to visualize historical 
            trends in allowances allocated, deducted, and banked for active 
            allowance trading programs operated by EPA's Clean Air Markets Division.")),
        ),
      sidebarLayout(
      sidebarPanel(width=3,
        h2("Filters"), 
                   radioButtons(ns("levelOfAnalysis"), "1. Level of Analysis",
                                c("Program"="p", 
                                  "State"="s"),
                                selected = "p"
                                ),
                   conditionalPanel(
                     condition = "input.levelOfAnalysis == 'p'", ns = ns,
                     dropdownSelectUI(ns("programInput"), 
                                      label= paste0("2. ",singleLabelConversion$label[singleLabelConversion$columnName == "programDescription"]),
                                      placeholder_label= "--select program--",
                                      choices= sort(unique(programInfo$currentCompliancePrograms$programCode[!is.na(programInfo$currentCompliancePrograms$complianceYears)])))
                     ),
                   conditionalPanel(
                     condition = "input.levelOfAnalysis == 's'", ns = ns,
                     columnFilterSetUI(ns("filterset"), 
                                       filterIndices$allowanceBank
                                       )
                              
                     ),
        div(class="clear-preview-btns",
            actionButton(ns("clearFilters"), "Clear Filters"),
            actionButton(class="preview-button", ns("previewButton"), "Preview Data")
            )
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
  
  selectedDataSet <- reactive({applicableAllowanceCompliance$data})
  choices <- reactiveValues(programsSelected=NULL,
                            statesSelected=NULL)
    
  filterSetReturn <- callModule(columnFilterSet, "filterset", 
                                df = selectedDataSet, 
                                filterIndices$allowanceBank,
                                reactive(c(input$clearFilters)),
                                startIndexAdd = 1,
                                programIsSingleSelect=TRUE)
    
  filterSingleReturn <- callModule(dropdownSelectServer,"programInput",
                                   df = selectedDataSet,
                                   columnToFilter="programDescription",
                                   clearEvent = reactive(c(input$clearFilters)))
  
  observeEvent(c(input$clearFilters),{
    output$programPlots <- renderUI({
      NULL
    })
    callModule(dataTableSever,"programTable", 
               "", NULL, "")
  },ignoreInit = TRUE)
  
  observeEvent(c(filterSetReturn$selections, filterSingleReturn$selections),{
    if (is.null(input$levelOfAnalysis) | length(input$levelOfAnalysis)==0)
      return()
    if (input$levelOfAnalysis == "p"){
      choices$programsSelected <- filterSingleReturn$selections
    }
    else{
      choices$programsSelected <- filterSetReturn$selections[["programCode"]]
      choices$statesSelected <- filterSetReturn$selections[["stateName"]]
    }
  })
    
  observeEvent(input$previewButton,{
    #programsSelected <- currentCompliancePrograms$programCode[currentCompliancePrograms$programDescription %in% 
    #                                          choices$programsSelected]
    programsSelected <- choices$programsSelected
    statesSelected <- statesMdm$stateCode[statesMdm$stateName %in% choices$statesSelected]
    
    if (length(programsSelected) == 0){
      if (input$levelOfAnalysis == "s"){
        showModal(modalDialog(
          title = "Input missing",
          "Please make a state and program selection.",
          easyClose = TRUE
        ))
      }
      else {
        showModal(modalDialog(
          title = "Input missing",
          "Please make a program selection.",
          easyClose = TRUE
        ))
      }
      return()
    }
    
    complianceYears <- programInfo$currentCompliancePrograms[programInfo$currentCompliancePrograms$programCode %in%
                                                   programsSelected,]$complianceYears
    minimumYear <- min(unlist(complianceYears))
    maximumYear <- max(unlist(complianceYears))
    
    # Get everything aggregated to the state and
    #   fix allowances per csapr and arp
    
    csaprAllocations <- unique(subset(csaprStateBudgets, select = -c(assuranceFlag, variabilityLimit, assuranceLevel, programDescription)))
    
    csaprAllocations <- csaprAllocations[csaprAllocations$programCode %in% programsSelected & 
                                           csaprAllocations$year <= programInfo$latestComplianceYear,]
    
    
    if (input$levelOfAnalysis == "s"){
      if (length(statesSelected) == 0){
        showModal(modalDialog(
          title = "Input missing",
          "Please make a state and program selection.",
          easyClose = TRUE
        ))
        return()
      }
      
      if (programsSelected == "ARP"){
        if (is.null(ARPComplianceData$stateLevel)){
          load_ARP_data()
        }
        aggregatedComplianceData <- ARPComplianceData$stateLevel[ARPComplianceData$stateLevel$stateCode %in% statesSelected,]
      }
      else {
        facilityComplianceData <- get_allow_comp_data(
          complianceYears = seq(minimumYear,maximumYear),
          programs = programsSelected, states = statesSelected)
        
        aggregatedComplianceData <- getStatePlotData(facilityComplianceData,
                                                     c("allocated", 
                                                       "totalAllowancesDeducted",
                                                       "carriedOver"))
      }
      
      if (programsSelected %in% unique(csaprStateBudgets$programCode)){
        aggregatedComplianceData <- merge(csaprAllocations[,c("stateName","programCode","year","allocated")],
              subset(aggregatedComplianceData, select =-c(allocated) ),
              by=c("stateName","programCode","year"))
        aggregatedComplianceData$carriedOver <- aggregatedComplianceData$allocated - aggregatedComplianceData$totalAllowancesDeducted
      }
      
      # order for scatter plot
      aggregatedComplianceData <- aggregatedComplianceData[order(aggregatedComplianceData$year),]
      
      # data for summary table
      tableData <- data.frame(stateName=aggregatedComplianceData$stateName, 
                              programCode=aggregatedComplianceData$programCode,
                              year=aggregatedComplianceData$year, 
                              allocated=aggregatedComplianceData$allocated, 
                              totalAllowancesDeducted=aggregatedComplianceData$totalAllowancesDeducted, 
                              carriedOver=aggregatedComplianceData$carriedOver)
      
      names(tableData) <- tableLabelConversion$label[match(names(tableData), 
                                                           tableLabelConversion$columnName)]
      statestrg <- paste( unlist(statesSelected), collapse='-')
      filename <- paste0("state-level-complaince-data-",statestrg,".csv")
      
      output$programPlots <- renderUI({
        tagList(
          p("Expand the boxes below by clicking the minus button 
            to see the respective plots. 
            To remove a line from the graph click on the legend item 
            on the right hand side. To add it back to the plot, click the item again."),
          #plotlyOutput(session$ns("plots"))
          box(lineGraphUI(session$ns("allocatedPlot")), 
              title = "Allowances Allocated",  
              collapsible = TRUE, 
              collapsed = TRUE,
              status = "primary",
              width = 12),
          box(lineGraphUI(session$ns("deductedPlot")), 
              title = "Allowances Deducted",  
              collapsible = TRUE, 
              collapsed = TRUE,
              status = "primary",
              width = 12),
          box(lineGraphUI(session$ns("carriedPlot")), 
              title = "Allowances Banked",  
              collapsible = TRUE, 
              collapsed = TRUE,
              status = "primary",
              width = 12),
        )
      })
      
      callModule(lineGraphSever, "allocatedPlot", 
                 df=aggregatedComplianceData, xVals="year", yVals="allocated", 
                 group="stateName", graphTitle="Allowances Allocated", 
                 xtitle="Years", ytitle="Number of Allowances", 
                 hoverText=paste("Program: ", aggregatedComplianceData$programCode,
                                 "<br>State: ", aggregatedComplianceData$stateName,
                                 "<br>Allocated: ", aggregatedComplianceData$allocated,
                                 "<br>Year: ", aggregatedComplianceData$year),
                 xAxisIsYears=TRUE)
        
      callModule(lineGraphSever, "deductedPlot", 
                 df=aggregatedComplianceData, xVals="year", yVals="totalAllowancesDeducted", 
                 group="stateName", graphTitle="Allowances Deducted", 
                 xtitle="Years", ytitle="Number of Allowances", 
                 hoverText=paste("Program: ", aggregatedComplianceData$programCode,
                                 "<br>State: ", aggregatedComplianceData$stateName,
                                 "<br>Deducted: ", aggregatedComplianceData$totalAllowancesDeducted,
                                 "<br>Year: ", aggregatedComplianceData$year),
                 xAxisIsYears=TRUE)
        
      callModule(lineGraphSever, "carriedPlot", 
                 df=aggregatedComplianceData, xVals="year", yVals="carriedOver",
                 group="stateName", graphTitle="Allowances Banked", 
                 xtitle="Years", ytitle="Number of Allowances", 
                 hoverText=paste("Program: ", aggregatedComplianceData$programCode,
                                 "<br>State: ", aggregatedComplianceData$stateName,
                                 "<br>Banked: ", aggregatedComplianceData$carriedOver,
                                 "<br>Year: ", aggregatedComplianceData$year),
                 xAxisIsYears=TRUE)
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
      
      complianceColumns <- c("allocated", 
                             "totalAllowancesDeducted",
                             "carriedOver")
      
      if (programsSelected == "ARP"){
        if (is.null(ARPComplianceData$stateLevel)){
          load_ARP_data()
        }
        stateARPcomplianceData <- ARPComplianceData$stateLevel 
        aggregatedComplianceData <- aggregate(stateARPcomplianceData[,c("allocated", 
                                                                        "totalAllowancesDeducted",
                                                                        "carriedOver")],
                                              list(stateARPcomplianceData$programCode,
                                                   stateARPcomplianceData$year),
                                              sum)
        colnames(aggregatedComplianceData)[1:2] <- c("programCode","year")
        tableData = merge(x=aggregatedComplianceData,
                                         y=programInfo$currentCompliancePrograms[,c("programCode",
                                                                                    "programDescription")],
                                         by.x="programCode")
      }
      else {
        facilityComplianceData <- get_allow_comp_data(
          complianceYears = seq(minimumYear,maximumYear),
          programs = programsSelected)
        
        names(facilityComplianceData)[names(facilityComplianceData) == 
                                        'programCodeInfo'] <- 'programCode'
        
        tableData <- getProgramTableData(facilityComplianceData, 
                                         complianceColumns)
      }
      
      if (programsSelected %in% unique(csaprStateBudgets$programCode)){
        programDF <- csaprAllocations[csaprAllocations$programCode == programsSelected,]
        aggregatedAllocationsProgramDF <- aggregate(allocated~year+programCode,data=programDF,sum)
        
        tableData <- merge(aggregatedAllocationsProgramDF[,c("programCode","year","allocated")],
                                          subset(tableData, select =-c(allocated) ),
                                          by=c("programCode","year"))
        tableData$carriedOver <- tableData$allocated - tableData$totalAllowancesDeducted
      }
      
      output$programPlots <- renderUI({
        tagList(
          lineGraphUI(session$ns("programPlot"))
        )
      })
      
      rowDisplayColumnNames <- tableLabelConversion$label[match(complianceColumns,tableLabelConversion$columnName)]
      
      names(tableData) <- tableLabelConversion$label[match(names(tableData), 
                                                           tableLabelConversion$columnName)]
      
      programPlotData <- getPlotDataByColumns(tableData,rowDisplayColumnNames)
      
      # order for scatter plot
      programPlotData <- programPlotData[order(programPlotData$Year),]
      
      callModule(lineGraphSever, "programPlot", 
                 df=programPlotData, xVals='Year', yVals='Value',
                 group='Category', graphTitle=paste0(choices$programsSelected,
                                                     " Allowance Information"),
                 xtitle="Years", ytitle="Number of Allowances", 
                 hoverText=paste("Category: ", programPlotData$Category,
                                 "<br>Value: ", programPlotData$Value,
                                 "<br>Year: ", programPlotData$Year),
                 xAxisIsYears=TRUE)
      
      filename <- paste0("program-level-complaince-data-",programsSelected,".csv")
    }
    
    callModule(dataTableSever,"programTable", 
               "Summary Data Table", tableData, filename)
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
                           y=statesMdm[,c("stateCode","stateName")],
                           by.x="stateCode")
    
    aggregatedComplianceData = merge(x=aggregatedData,
                                     y=programInfo$currentCompliancePrograms[,c("programCode",
                                                                                "programDescription")],
                                     by.x="programCode")
    
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
                                     y=programInfo$currentCompliancePrograms[,c("programCode",
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


