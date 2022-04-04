### Server
function(input, output, session) {
  
  output$allowanceBankText <- renderText({
    "..."
  })
  
  output$programBugetsText <- renderText({
    "..."
  })
  
  selectedDataSet <- reactive({applicableAllowCompTable})
  filteredData <- reactiveVal(applicableAllowCompTable)
  choices <- reactiveValues(programsSelected=NULL,
                                    statesSelected=NULL)
  
  filterSetReturn <- callModule(columnFilterSet, "filterset", 
                                df = selectedDataSet, 
                                allowanceBankFilterIndicesState)
  
  filterSingleReturn <- callModule(multiSelectServer,"programInput",
                                df = selectedDataSet,
                                columnToFilter="programDescription")
  
  
  observeEvent(c(filterSetReturn$selections, filterSingleReturn$selections),{
    if (input$levelOfAnalysis == "p"){
      choices$programsSelected <- filterSingleReturn$selections[["programDescription"]]
      filteredData(filterSingleReturn$data)
    }
    else{
      choices$programsSelected <- filterSetReturn$selections[["programDescription"]]
      choices$statesSelected <- filterSetReturn$selections[["stateName"]]
      filteredData(filterSetReturn$data)
    }
  })
  
  observeEvent(input$previewButton,{
    programsSelected <- allCompliancePrograms$programCode[allCompliancePrograms$programDescription %in% choices$programsSelected]
    statesSelected <- states$stateCode[states$stateName %in% choices$statesSelected]
    
    complianceYears <- allCompliancePrograms[allCompliancePrograms$programCode %in% programsSelected,]$complianceYears
    minimumYear <- min(unlist(complianceYears))
    maximumYear <- max(unlist(complianceYears))
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
      allocatedData <- getStatePlotDataByCol(facilityComplianceData,
                                             "allocated", "Allowances Allocated")
      
      callModule(lineGraphSever, "allocatedPlot", 
                 df=allocatedData, xVals='Year', yVals='Value', 
                 group='State', graphTitle="Allowances Allocated", 
                 xtitle="Years", ytitle="Number of Allowances", 
                 xAxisIsYears=TRUE)
      
      deductedData <- getStatePlotDataByCol(facilityComplianceData,
                                             "totalAllowancesDeducted", "Allowances Deducted")
      
      callModule(lineGraphSever, "deductedPlot", 
                 df=deductedData, xVals='Year', yVals='Value', 
                 group='State', graphTitle="Allowances Deducted", 
                 xtitle="Years", ytitle="Number of Allowances", 
                 xAxisIsYears=TRUE)
      
      carriedData <- getStatePlotDataByCol(facilityComplianceData,
                                            "carriedOver", "Allowances Banked")
      
      callModule(lineGraphSever, "carriedPlot", 
                 df=carriedData, xVals='Year', yVals='Value', 
                 group='State', graphTitle="Allowances Banked", 
                 xtitle="Years", ytitle="Number of Allowances", 
                 xAxisIsYears=TRUE)
      
      output$bankTable <- renderDataTable( 
        facilityComplianceData,
        options = list(scrollX = TRUE))
      
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
      output$bankTable <- renderDataTable( 
        facilityComplianceData,
        options = list(scrollX = TRUE))
    }
  })
  
  getStatePlotDataByCol <- function(dataframe, column, label){
    
    aggregatedData <- aggregate(dataframe[[column]],
                               list(dataframe$programCode,
                                    dataframe$stateCode,
                                    dataframe$year),
                               sum)
    colnames(aggregatedData) <- c("Regulatory Program","stateCode",
                                 "Year","Value")
    aggregatedData$Category <- label
    
    aggregatedData = merge(x=aggregatedData,
                          y=states[,c("stateCode","stateName")],
                          by="stateCode")
    names(aggregatedData)[names(aggregatedData) == 'stateName'] <- 'State'
    aggregatedData
    
  }
  
  getPlotDataAll <- function(facilityComplianceData){
    columnNameToAgg <- c("allocated","complianceYearEmissions","carriedOver")
    labelNameToAgg <- c("Allowances Allocated","Allowances Deducted","Allowances Carried Over")
    
    labelConversionNameToAgg <- data.frame(columnName, label)
    
    graphingValues <- c("allocated","complianceYearEmissions","carriedOver")
    
    plotData <- bind_rows(lapply(labelConversion$columnName, function(i){
      data <- aggregate(facilityComplianceData[[i]],
                        list(facilityComplianceData$stateCode,
                             facilityComplianceData$year),
                        sum)
      colnames(data) <- c("stateCode", "Year", "Value")
      data$Category <- labelConversion$label[labelConversion$columnName==i]
      data
    }))
    
    plotData = merge(x=plotData,
                     y=states[,c("stateCode","stateName")],
                     by="stateCode")
    plotData
  }
  
  #Not needed
  getFacilityOrUnitComplianceData <- observe({
    #print(head(filteredData()))
    #programs <- unique(filteredData()$programCode)
    #states <- unique(filteredData()$stateCode)
    #complianceYears <- allCompliancePrograms[allCompliancePrograms$programCode %in% programs,]$complianceYears
    #minimumYear <- min(unlist(complianceYears))
    #maximumYear <- max(unlist(complianceYears))
    #print(minimumYear)
  })
  
}