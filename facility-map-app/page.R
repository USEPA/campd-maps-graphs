### UI

facilityMapAppUI <- function(id){
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
      titlePanel("Facility Map"),
      tags$hr(),
      
      fluidRow(column(10, h4("Welcome to the Facility Map!"),
                      tags$ul(
                        tags$li('The following map shows where fossil 
                                fuel-powered power plants with the 
                                generation capacity greater than or equal 
                                to 25 megawatts are located.'),
                        tags$li('Each facility on this map is regulated under 
                                one or more allowance trading programs. 
                                Click on "Facility Summary" to view which 
                                program(s) the plant falls under, as well as 
                                its individual operating unit and fuel type 
                                breakdown.'),
                        tags$li('Facilities must meet compliance by program 
                                specific compliance deadlines. Click on 
                                "Account Summary" to evaluate whether a 
                                facility was compliant in the most recent 
                                compliance year comparing the number of 
                                allowances the facility held against its 
                                emissions.')
                      )),
               column(2, actionButton(ns("stopanimation"), "Stop Animation"))
               ),
      fluidRow(
        columnFilterSingleSelectSetUI(ns("filterset"), facilityFilterIndices),
      ),
      #GET RID OF STYLE!
      fluidRow(tags$style(type = "text/css", "#map {!important;}"),
               style = "padding-bottom: 10px;",
               absolutePanel(id = "elements", class = "panel panel-default",
                             draggable = TRUE,
                             right = 0,
                             style = "overflow-y:scroll; max-height: 380px;
                             background-color: white; z-index: 10; margin: 10px;
                             padding: 10px;",
                             tabsetPanel(
                               tabPanel("Facility Summary", style = "margin: 10px; height:100%;
                                        width:300px;",
                                        uiOutput(ns("fac_summary_text")),
                               ),
                               tabPanel("Account Summary", style = "margin: 10px; height:100%;
                                        width:300px;",
                                        uiOutput(ns("acct_summary_text")),
                               ),
                             )
               ),
               leafletOutput(ns("map"),width="100%")
      ),
      p("The numbered bubbles represent the density of facilities in a 
        particular area. Zoom in and click on individual pins on the map 
        to view facility specific summary data.")
    )
  )
}

facilityMapAppServer <- function(input, output, session) {
  
  reactiveFacilityData <- reactive({programfacilityData})
  #dataForMap <- reactiveVal(programfacilityData)
  
  filterSetReturn <- callModule(columnFilterSingleSelectSet, "filterset", 
                                df = reactiveFacilityData, 
                                facilityFilterIndices)
  
  observeEvent(input$stopanimation, {
    stop_gif()
  })
  
  observeEvent(c(input$map_click,filterSetReturn$selections),{
    output$fac_summary_text <- renderUI({HTML("")})
    output$acct_summary_text <- renderUI({HTML("")})
  },ignoreInit = TRUE)
  
  # initial map
  observeEvent(filterSetReturn$selections,{
    output$map <- renderLeaflet({
      leaflet() %>%
        addTiles() %>% addMarkers(data = filterSetReturn$data,
                                  lng = ~longitude, lat = ~latitude, 
                                  layerId = ~facilityId,
                                  clusterOptions = markerClusterOptions()) %>% 
        #setView(lng = -93.85, lat = 37, zoom = 4)
        fitBounds(lng1 = min(filterSetReturn$data$longitude)-1.5, 
                  lat1 = min(filterSetReturn$data$latitude)-1.5,
                  lng2 = max(filterSetReturn$data$longitude)+1.5,
                  lat2 = max(filterSetReturn$data$latitude)+1.5)
    })
  },ignoreInit = TRUE)
  
  # When map is clicked, show a popup with facility info
  observeEvent(input$map_marker_click,{
    event <- input$map_marker_click
    if (is.null(event)){
      return()
    }
    isolate({
      showFacInfoPopup(event$id, event$lat, event$lng)
    })
    output$fac_summary_text <- renderUI({
      get_facility_info_for_side_panel(event$id)
    })
    output$acct_summary_text <- renderUI({
      get_compliance_info_for_side_panel(event$id)
    })
  })
  
  # Show a popup at the given location
  showFacInfoPopup <- function(fac_id, lat, lng) {
    # Pick first row of unit data
    selectedFac <- unique(facilityData[facilityData$facilityId == fac_id,])[1,]
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
  get_facility_info_for_side_panel <- function(facilityId){
    
    selectedFac <- unitData[unitData$facilityId == facilityId,]
    strFuelTypes <- paste0(unique(unitData$primaryFuelInfo[unitData$facilityId == facilityId]),collapse = ", ")
    
    content <- 
      tagList(
        tags$h4(selectedFac$facilityName[1]),
        tags$strong("Facility ID:"),sprintf(" %s", as.character(facilityId)), tags$br(),
        tags$strong("Primary Fuel Types:"),sprintf("%s", strFuelTypes)
      )
  }
  
  get_compliance_info_for_side_panel <- function(facilityId){
    
    selectedFac <- unitData[unitData$facilityId == facilityId,]
    complianceFacilityData <- get_allow_comp_data(latestComplianceYear,facilities=c(facilityId))
    
    if(is.null(complianceFacilityData)){subjectedPrograms <- "None"}
    else{subjectedPrograms <- paste0(unique(complianceFacilityData$programCodeInfo),collapse = ", ")}
    
    
    content <- 
      tagList(
        tags$h4(selectedFac$facilityName[1]),
        tags$strong("Facility ID:"),sprintf(" %s", as.character(facilityId)), tags$br(),
        tags$strong("Allowance Programs for Compliance:"),sprintf("%s", subjectedPrograms),
        #tags$div(class="summarytable",
        #  tags$div(class="tabletitle",
        #           "Car Name"),
        #  tags$div(
        #    column(6,"row1"),
        #    column(6,"row2")
        #  ),
        #  tags$div(class="tabletitle",
        #           "Car Name2"),
        #  tags$div(
        #    column(6,"row3"),
        #    column(6,"row4")
        #  )
        #)
      )
    
    
    content
    
  }
  
  # update map using leaflet proxy 
  # use observe to listen to filter changes
  observeEvent(filterSetReturn$selections,{
    # clear map
    map <- leafletProxy("map",data=filterSetReturn$data) %>% 
      clearMarkerClusters() %>%
      clearMarkers() %>%  clearShapes() %>% clearControls() %>%
      addTiles() %>%
      addMarkers(data = filterSetReturn$data,
                 lng = ~longitude, lat = ~latitude, 
                 layerId = ~facilityId,
                 clusterOptions = markerClusterOptions()) %>%
      leaflet.extras::addSearchOSM(options = searchOptions(hideMarkerOnCollapse = TRUE))
  },ignoreInit = TRUE)
}


