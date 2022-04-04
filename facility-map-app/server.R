
# server definition
server <- function(session, input, output) {
  
  # listen to filter changes
  toListen <- reactive({
    list(input$program,input$fuelType,input$clearbutton)
  })
  
  # keep track of map clicks and filters to clear necessary UI elements
  click_data <- reactiveValues(clickedMarker=NULL)
  
  observeEvent(input$map_marker_click,
               {click_data$clickedMarker <- input$map_marker_click})
  
  observeEvent(c(toListen(),input$map_click),{
    click_data$clickedMarker <- NULL
    leafletProxy("map") %>% clearPopups()})
  
  output$clickInfo <- renderPrint({click_data$clickedMarker})
  
  # for maintaining the state of data/parameter type selected
  emiss_data <- reactiveVal()
  parameter_type <- reactiveVal()
  
  # update selections in program filter
  updateSelectizeInput(session, 
                       "program", 
                       choices = get_programs(fac_data,year_for_ploting)$prg_name)
  
  # clear filters when clearbutton is pressed
  observeEvent(input$clearbutton, {
    updateSelectizeInput(session, "program", options = list(placeholder = '--Select Program--',
                                  onInitialize = I('function() { this.setValue(""); }')))
    updateSelectizeInput(session, "fuelType", options = list(placeholder = '--Select Fuel Type--',
                                                            onInitialize = I('function() { this.setValue(""); }')))
  })
  
  # initial map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37, zoom = 4)
  })
  
  # When map is clicked, show a popup with facility info
  observeEvent(input$map_marker_click,{
    event <- input$map_marker_click
    if (is.null(event)){
      return()
    }
    isolate({
      showFacInfoPopup(event$id, event$lat, event$lng)
    })
  })
  
  # Show a popup at the given location
  showFacInfoPopup <- function(fac_id, lat, lng) {
    selectedFac <- fac_data[fac_data$facilityId == fac_id,][1,]
    content <- as.character(
      tagList(
        tags$h4(selectedFac$facilityName),
        tags$strong(HTML(sprintf("%s, %s",
                                 selectedFac$county, 
                                 as.character(selectedFac$stateCode)
        ))), tags$br(),
        sprintf("Latitude: %s", as.character(lat)), tags$br(),
        sprintf("Longitude: %s", as.character(lng))
      ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = fac_id)
  }
  
  # collect and output facility information for display
  output$fac_summary_text <- renderUI({
    event <- input$map_marker_click
    if (is.null(click_data$clickedMarker) | is.null(event)){
      HTML("")
      return()
    }
    fac_id <- event$id
    selectedFac <- data[data$facilityId == fac_id,]
    
    last_op_year <- max(selectedFac$year)
    
    ownoplist <- paste(unique(selectedFac$ownerOperator))
    if (length(unique(selectedFac$ownerOperator))>1){
      ownoplist <- c()
      for (item in unique(selectedFac$ownerOperator)){
        ownoplist <- unique(c(ownoplist, c(strsplit(item, ","))))
      }
      ownoplist <- paste(unique(do.call(c, ownoplist)), collapse=', ')
    }
    
    op_by_unit <- ""
    for (row in 1:nrow(selectedFac)){
      stg <- ""
      if(!is.null(selectedFac[row,]$primaryFuelInfo))
        {stg <- paste0(stg, selectedFac[row,]$primaryFuelInfo, ", ")}
      if(!is.null(selectedFac[row,]$secondaryFuelInfo))
        {stg <- paste0(stg, selectedFac[row,]$secondaryFuelInfo, ", ")}
      op_by_unit <- paste0(op_by_unit,"&nbsp;&nbsp;&nbsp;&nbsp;Unit ", 
                           selectedFac[row,]$unitId, ": ",
                           stg, selectedFac[row,]$unitType,
                           "<br/>")
    }
    
    #percent_diff <- get_emissions_percent_string(years,fac_id)
    
    content <- as.character(
      tagList(
        tags$h4(selectedFac$facilityName[1]),
        tags$strong("Facility ID:"),sprintf(" %s", as.character(fac_id)), tags$br(),
        #tags$strong("Percent emissions change:"),sprintf(" %s", percent_diff), tags$br(),
        tags$strong("Owner/Operator:"),sprintf(" %s", as.character(ownoplist)), tags$br(),
        tags$strong("Unit Information:"),HTML(sprintf("<br/>%s", op_by_unit))
      ))
    
    
    HTML(content)
    
  })
  
  # collect and output account information for display
  output$acct_summary_text <- renderUI({
    event <- input$map_marker_click
    if (is.null(click_data$clickedMarker) | is.null(event)){
      HTML("")
      return()
    }
    fac_id <- event$id
    
    holding_data <- get_allow_holding_data(fac_id)
    
    comp_data <- get_allow_comp_data(year_for_ploting, fac_id)
    
    comp_by_prg <- ""
    for (row in 1:nrow(comp_data)){
      comp_by_prg <- paste0(comp_by_prg,"&nbsp;&nbsp;&nbsp;&nbsp;", 
                            comp_data[row,]$programCodeInfo, ": ")
      if(!is.na(comp_data[row,]$excessEmissions)){
        comp_by_prg <- paste0(comp_by_prg,"Not in compliance", "<br/>")
      }
      else{
        comp_by_prg <- paste0(comp_by_prg,"In compliance", "<br/>")
      }
    }
    
    holding_by_prg <- ""
    for (row in 1:nrow(holding_data)){
      holding_by_prg <- paste0(holding_by_prg,"&nbsp;&nbsp;&nbsp;&nbsp;", 
                               holding_data[row,]$programCodeInfo, ": ")
      holding_by_prg <- paste0(holding_by_prg,holding_data[row,]$x, "<br/>")
    }
    
    content <- as.character(
      tagList(
        tags$h4(comp_data$facilityName[1]),
        tags$strong("Facility ID:"),sprintf(" %s", as.character(fac_id)), tags$br(),
        tags$strong("Compliance year:"),sprintf(" %s", as.character(year_for_ploting)), tags$br(),
        tags$strong("Allowance Holdings:"),HTML(sprintf("<br/>%s", holding_by_prg)),
        tags$strong("Program Compliance:"),HTML(sprintf("<br/>%s", comp_by_prg))
      ))
    
    
    HTML(content)
  })
  
  # update map using leaflet proxy 
  # use observe to listen to filter changes
  observeEvent(toListen(), {
    # clear map
    map <- leafletProxy("map") %>% 
      clearMarkerClusters() %>%
      clearMarkers() %>%  clearShapes() %>% clearControls() %>%
      addTiles() %>%
      leaflet.extras::addSearchOSM(options = searchOptions(hideMarkerOnCollapse = TRUE))
    
    # CODE BETTER!!! getting data for map
    if (input$program == ""){
      data_for_map <- fac_data
    }
    else {
      if (input$fuelType != ""){
        data_for_map <- fac_data[grepl(input$fuelType, fac_data$primaryFuelInfo, fixed = TRUE),]
        data_for_map <- rbind(data_for_map, fac_data[grepl(input$fuelType, fac_data$secondaryFuelInfo, fixed = TRUE),])
      }
      else {data_for_map <- fac_data}
      prg_code <- program_names[program_names$prg_name == input$program,]$prg_code
      data_for_map <- data_for_map[grepl(prg_code, data_for_map$programCodeInfo, fixed = TRUE),]
      
    }
    # CODE BETTER!!! getting data for map
    if (input$fuelType != ""){
      pri_data_for_map <- data_for_map[grepl(input$fuelType, data_for_map$primaryFuelInfo, fixed = TRUE),]
      data_for_map <- rbind(pri_data_for_map, data_for_map[grepl(input$fuelType, data_for_map$secondaryFuelInfo, fixed = TRUE),])
    }
    
    data_for_map <- get_facilities_lat_long(data_for_map)
    
    #add markers to map with cluster option
    map %>% addTiles() %>% addMarkers(data = data_for_map,
                       lng = ~longitude, 
                       lat = ~latitude, 
                       layerId = ~facilityId,
                       clusterOptions = markerClusterOptions())
  })
  
  # parameter selection
  observeEvent(input$parameter_type,{ 
    parameter_type(input$parameter_type)
  })
  
  # set emissions data for graph
  observeEvent(input$map_marker_click,{
    event <- input$map_marker_click
    if (is.null(event))
      return()
    emiss_data(get_annual_fac_emissions_data(years, event$id))
  })
  
  # plot emissions data 
  output$emiss_plot <- renderPlotly({
    event <- input$map_marker_click
    if (is.null(click_data$clickedMarker) | is.null(event)){
      return(NULL)
    }
    parameter_type <- parameter_type()
    emiss_data <- emiss_data()
    meas_unit <- measure_units[measure_units$parameter_type == parameter_type,]$measure_unit
    
    fig <- plot_ly(x = as.character(emiss_data$year), 
                     y = emiss_data[,parameter_type], 
                     name = emiss_data$unitId, type = 'bar') %>% 
      layout(title = "Yearly Emissions",
               legend=list(title=list(text='<b> Unit ID </b>')),
               xaxis = list(title = "Year"),
               yaxis = list(title = paste0(parameter_type," ",meas_unit)), 
               barmode = 'stack')
    
  })
  
  # download emissions data button
  output$downloademissData <- downloadHandler(
       filename = function() {
         paste0(emiss_data()$facilityName[1], "-", 
                year_for_ploting,'-emissions-data-', 
                Sys.Date(), '.csv')
       },
       content = function(file) {
         write.csv(emiss_data(), file)
       }
     )
}