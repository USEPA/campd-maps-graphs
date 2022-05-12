### UI

cluster_icon <- tags$img(src='cluster-icon.png', width='35px',height='35px')

facilityMapAppUI <- function(id){
  ns <- NS(id)
  tagList(
    div(class="banner",
        #p("content"),
        div(class="banner-conents",
            "Facility Map"),
    ),
    fluidPage(
      useShinyjs(),
      useShinydashboard(),
      includeScript('www/map-script.js'),
      
      tags$main(h1("Welcome to the Facility Map!")),
      tags$head(HTML("<title>Facility Map</title>")), 
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
      
      h2("Getting Started"),
      p(class="intro-text","This facility map uses data collected as part 
        of EPA's emissions trading programs. If you are interested in 
        exploring the data further, check out [link-CAMPD]!"),
      p(class="intro-text","Click on a pin marker",
        tags$img(src='https://unpkg.com/leaflet@1.3.1/dist/images/marker-icon-2x.png', 
                 width='20px',height='35px',alt = "Leaflet marker icon"),
        "on the map to view a facility's basic information, 
        as well as its program compliance using the side panel's ",
        '"Facility Summary" and "Compliance Summary" expandable boxes, respectively.'),
      div(class="bullet-title","Using the Map"),
      tags$ul(class="intro-list",
        tags$li('This map refreshes dynamically.'),
        tags$li('Use the state and county search to find facilities near you.'),
        tags$li(class="indent",'If a state is selected first in the search box, the county 
            search will be narrowed down to those in the state. If county is selected first, the state 
            search will be narrowed down to the state which the county is in.'),
        tags$li(class="indent",'Note that the county search is grouped by state.'),
        tags$li('Use the facility filer to narrow down facilities by program.'),
        tags$li('Use the clear buttons to clear your filters or searches.'),
        tags$li('Facility Summary displays basic information about the facility chosen.'),
        tags$li('Compliance Summary displays compliance 
                  information for programs applicable to the chosen facility.'),
      ),
      tags$hr(),
      p(class="intro-text","For more information on these programs visit",
        tags$a(href="https://www.epa.gov/airmarkets/programs", 
               "EPA's Clean Air Markets Programs web area",
               target="_blank"),
        "."),
      
      fluidRow(id="filtersearch-container",
        column(8, 
               fluidRow(class='column-decorator', div(h3('Location Search',
                                                         bsButton("search-tooltip", label = "Search Tooltip", 
                                                                  icon = icon("question"), style = "info", 
                                                                  size = "extra-small"))),
                        bsPopover(id = "search-tooltip", title = "Using the searches",
                                  content = paste0("Use the search dropdown to highlight specific locations and zoom into the", 
                                                   " location to find nearby facilities."),
                                  placement = "right", 
                                  trigger = "focus", 
                                  options = list(container = "body")
                        ),
                   fluidRow(
                     column(5,
                            searchUI(ns('stateSearchInput'), 
                                     placeholder='--search by state-- ', 
                                     label='State Name',
                                     df=state_sf, 
                                     group="", 
                                     items='stateName')),
                     column(5,
                            searchUI(ns('countySearchInput'), 
                                     placeholder='--search by county-- ', 
                                     label='County Name',
                                     df=countyState, 
                                     group='stateName', 
                                     items='countyName'))
                     ),
                   
                   div(class="select-clear", actionButton(ns("clearSearch"), "Clear Search"))
                   
               )
        ),
        column(4, 
               fluidRow(class="column-decorator", div(h3("Facility Filters",
                                                         bsButton("filter-tooltip", label = "Filter Tooltip", 
                                                                  icon = icon("question"), style = "info", 
                                                                  size = "extra-small"))),
                        bsPopover(id = "filter-tooltip", title = "Using the filter",
                                  content = paste0("Use the program filter to refine your search of facilities within a regulatory", 
                                                   " program across the United States."),
                                  placement = "right", 
                                  trigger = "focus", 
                                  options = list(container = "body")
                        ),
                        fluidRow(column(10,selectizeInput(ns("programSelection"), 
                                                          label=facilityMapLabelConversion$label[facilityMapLabelConversion$columnName == "programCode"],
                                                          choices=c("Select All",unique(na.omit(programfacilityData$programCode))),
                                                          selected=c("Select All"),
                                                          multiple = FALSE))),
                        div(class="select-clear", actionButton(ns("clearFilters"), "Clear Filters"))
               )
               
        )
      ),
      
      fluidRow(absolutePanel(id = "facility-map-panel", 
                             class = "panel panel-default",
                             draggable = TRUE,
                             div(id = "facility-summary-box", 
                                 box(uiOutput(ns("fac_summary_text")),
                                     title = "Facility Summary",  
                                     collapsible = TRUE, 
                                     status = "primary",
                                     width = 12)
                             ),
                             div(id = "compliance-summary-box", 
                                 box(uiOutput(ns("acct_summary_text")), 
                                     title = "Compliance Summary",  
                                     collapsible = TRUE, 
                                     status = "primary",
                                     width = 12)
                             )
               ),
               leafletOutput(ns("map"),width="100%", height = "500px")
      )
    )
  )
}

facilityMapAppServer <- function(input, output, session) {
  
  # State and county searches
  reactiveCountySearch <- reactive({countyState})
  stateFilterVal <- reactiveVal({NULL})
  countyFilterVal <- reactiveVal({NULL})
  markerData <- reactiveVal({programfacilityData})
  
  stateSearch <- callModule(searchServer,"stateSearchInput",
                            state_sf,reactive(c(input$clearSearch)),
                            filterBy='stateName',
                            filterVal=countyFilterVal)
  coutSearch <- callModule(searchServer,"countySearchInput",
                           countyState,reactive(c(input$clearSearch)),
                           filterBy='stateName',
                           filterVal=stateFilterVal)
  
  observeEvent(stateSearch(),{
    if (nrow(stateSearch()) != 0){
      # State search selected is filled to pass to county search in order to clear the
      # county search
      stateFilterVal(stateSearch()$stateName)
      # display county outline and zoom to location
      update_map_search(markerData(),state_sf,stateSearch(),'stateName','stateOutline')
      # clear program filter
      #updateSelectizeInput(
      #  session = session,
      #  inputId = "programSelection",
      #  selected=character(0))
    }
  },ignoreInit = TRUE)
  observeEvent(coutSearch(),{
    if (nrow(coutSearch()) != 0){
      # County search selected is filled to pass to state search in order to clear the
      # state search
      countyFilterVal(coutSearch()$stateName)
      # display county outline and zoom to location
      update_map_search(markerData(),countyState,coutSearch(),'countyns','countyOutline')
    }
  },ignoreInit = TRUE)
  # If something is selected in one of the searches, the map is cleared and
  # re-rendered, otherwise nothing happens
  observeEvent(input$clearSearch,{
    if (length(input$programSelection) != 0){
      if (input$programSelection == "Select All"){
        update_full_map(markerData())
      }
      else{
        shapeFileData <- state_sf[state_sf$stateName %in% unique(markerData()$stateName),]
        
        update_map_filter_selections(markerData(),shapeFileData)
      }
    }
  },ignoreInit = TRUE)
  
  # If program filter changes, and has a value, searches are cleared 
  # map is rendered to the facilities in the filter selections
  observeEvent(input$programSelection,{
    if (length(input$programSelection) != 0){
      if (input$programSelection != "Select All"){
        markerData(filter_facility_latlong_data(programfacilityData[programfacilityData$programCode == input$programSelection,]))
        
        if ((nrow(stateSearch()) != 0) | (nrow(coutSearch()) != 0)){
          update_map_with_shape_maintained(markerData())
        }
        else{
          shapeFileData <- state_sf[state_sf$stateName %in% unique(markerData()$stateName),]
          
          update_map_filter_selections(markerData(),shapeFileData)
        }
      }
      else{
        markerData(filter_facility_latlong_data(programfacilityData))
        if ((nrow(stateSearch()) != 0) | (nrow(coutSearch()) != 0)){
          update_map_with_shape_maintained(markerData())
        }
        else{
          update_full_map(markerData())
        }
        
      }
    }
  },ignoreInit = TRUE)
  # Clear filters and reinitialize the map
  observeEvent(input$clearFilters,{
    if (length(input$programSelection) != 0){
      if (input$programSelection != "Select All"){
        markerData(filter_facility_latlong_data(programfacilityData))
        if ((nrow(stateSearch()) != 0) | (nrow(coutSearch()) != 0)){
          update_map_with_shape_maintained(markerData())
        }
        else{
          update_full_map(markerData())
        }
        updateSelectizeInput(
          session = session,
          inputId = "programSelection",
          selected = c("Select All"))
        
      }
    }
  },ignoreInit = TRUE)
  
  # When map is clicked, or selection is made, clear popups and side panels
  observeEvent(c(input$map_click,input$programSelection,stateSearch(),coutSearch()),{
    event <- input$map_map_click
    output$fac_summary_text <- renderUI({HTML("")})
    output$acct_summary_text <- renderUI({HTML("")})
    leafletProxy("map") %>% clearPopups()
  },ignoreInit = TRUE)
  
  # Initialize leaflet
  output$map <- renderLeaflet({
    mapData <- filter_facility_latlong_data(programfacilityData)
    legendText <- tagList(div("The numbers inside the purple"),
                          div("circles indicate the amount of"),
                          div("facilities in that general area."),
                          p("The blue markers indicate one facility."))
    leaflet() %>%
      addTiles() %>% 
      addMarkers(data = mapData,
                 lng = ~longitude, lat = ~latitude, 
                 layerId = ~facilityId,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div><span>' + cluster.getChildCount() + '</div></span>',
                                               className: 'marker-cluster'
                                             });
                                           }"),
                                                       showCoverageOnHover=FALSE,
                                                       removeOutsideVisibleBounds=TRUE))%>%
      fitBounds(lng1 = min(na.omit(mapData$longitude))-1.5, 
                lat1 = min(na.omit(mapData$latitude))-1.5,
                lng2 = max(na.omit(mapData$longitude))+1.5,
                lat2 = max(na.omit(mapData$latitude))+1.5) %>%
      addControl(legendText, position = "bottomleft" )
  })
  
  # Update map and include shape file outline that was selected in the search
  update_map_search <- function(markerData,shapeFileData,searchRow,layer,group){
    bbox <- st_bbox(shapeFileData[searchRow,]) %>% 
      as.vector()
    leafletProxy("map",data=shapeFileData) %>% 
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearShapes() %>% 
      addTiles() %>% 
      addMarkers(data = markerData,
                 lng = ~longitude, lat = ~latitude, 
                 layerId = ~facilityId,
                 clusterOptions = markerClusterOptions(iconCreateFunction =
                                                         JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div><span>' + cluster.getChildCount() + '</div></span>',
                                               className: 'marker-cluster'
                                             });
                                           }"),
                                                       showCoverageOnHover=FALSE,
                                                       removeOutsideVisibleBounds=TRUE))%>%
      addPolygons(data = searchRow,
                  layerId = ~searchRow[[layer]], 
                  group = group,
                  color = 'green',
                  fillColor = 'green') %>% 
      fitBounds(bbox[1], bbox[2], bbox[3], bbox[4])
  }
  
  # Update map with filter selections - states outlined
  update_map_filter_selections <- function(markerData,shapeFileData){
    leafletProxy("map",data=shapeFileData) %>% 
      clearMarkerClusters() %>% clearPopups() %>%
      clearMarkers() %>% 
      clearShapes() %>% 
      addTiles() %>% addMarkers(data = markerData,
                                lng = ~longitude, lat = ~latitude, 
                                layerId = ~facilityId,
                                clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                        JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div><span>' + cluster.getChildCount() + '</div></span>',
                                               className: 'marker-cluster'
                                             });
                                           }"),
                                                                      showCoverageOnHover=FALSE,
                                                                      removeOutsideVisibleBounds=TRUE)) %>% 
      addPolygons(data = shapeFileData,
                  layerId = ~shapeFileData[['stateName']], 
                  group = 'stateOutline',
                  color = 'green',
                  fillColor = 'green') %>% 
      #setView(lng = -93.85, lat = 37, zoom = 4)
      fitBounds(lng1 = min(na.omit(markerData$longitude))-1.5, 
                lat1 = min(na.omit(markerData$latitude))-1.5,
                lng2 = max(na.omit(markerData$longitude))+1.5,
                lat2 = max(na.omit(markerData$latitude))+1.5)
  }
  # Update map with cleared filter selections
  update_full_map <- function(markerData){
    leafletProxy("map",data=markerData) %>% 
      clearMarkerClusters() %>% clearPopups() %>%
      clearMarkers() %>% 
      clearShapes() %>% 
      addTiles() %>% addMarkers(data = markerData,
                                lng = ~longitude, lat = ~latitude, 
                                layerId = ~facilityId,
                                clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                        JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div><span>' + cluster.getChildCount() + '</div></span>',
                                               className: 'marker-cluster'
                                             });
                                           }"),
                                                                      showCoverageOnHover=FALSE,
                                                                      removeOutsideVisibleBounds=TRUE)) %>% 
      #setView(lng = -93.85, lat = 37, zoom = 4)
      fitBounds(lng1 = min(na.omit(markerData$longitude))-1.5, 
                lat1 = min(na.omit(markerData$latitude))-1.5,
                lng2 = max(na.omit(markerData$longitude))+1.5,
                lat2 = max(na.omit(markerData$latitude))+1.5)
  }
  
  update_map_with_shape_maintained <- function(markerData){
    leafletProxy("map",data=markerData) %>% 
      clearMarkerClusters() %>% clearPopups() %>%
      clearMarkers() %>% 
      addTiles() %>% addMarkers(data = markerData,
                                lng = ~longitude, lat = ~latitude, 
                                layerId = ~facilityId,
                                clusterOptions = markerClusterOptions(iconCreateFunction =
                                                                        JS("
                                          function(cluster) {
                                             return new L.DivIcon({
                                               html: '<div><span>' + cluster.getChildCount() + '</div></span>',
                                               className: 'marker-cluster'
                                             });
                                           }"),
                                                                      showCoverageOnHover=FALSE,
                                                                      removeOutsideVisibleBounds=TRUE))
  }
  
  # When map is clicked, show a popup with facility info
  observeEvent(input$map_marker_click,{
    print(input$map_marker_click$id)
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
    selectedFac <- unique(facilityLatLongData[facilityLatLongData$facilityId == fac_id,])[1,]
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
    
    selectedUnitFac <- unitData[unitData$facilityId == facilityId,]
    selectedFac <- facilityLatLongData[facilityLatLongData$facilityId == facilityId,]
    fuelTypesStg <- paste0(unique(unitData$primaryFuelInfo[unitData$facilityId == facilityId]),collapse = ", ")
    operatingStatuses <- paste0(lapply(selectedUnitFac$unitId,function(unit){
      paste0("<strong>",unit,"</strong>",": ",selectedUnitFac$operatingStatus[selectedUnitFac$unitId == unit])
    }),collapse = "<br/>")
    
    if(length(na.omit(unitData$so2ControlInfo[unitData$facilityId == facilityId])) != 0){
      so2Controls <- "Yes"
    }
    else{so2Controls <- "No"}
    if(length(na.omit(unitData$noxControlInfo[unitData$facilityId == facilityId])) != 0){
      noxControls <- "Yes"
    }
    else{noxControls <- "No"}
    if(length(na.omit(unitData$pmControlInfo[unitData$facilityId == facilityId])) != 0){
      pmControls <- "Yes"
    }
    else{pmControls <- "No"}
    if(length(na.omit(unitData$hgControlInfo[unitData$facilityId == facilityId])) != 0){
      hgControls <- "Yes"
    }
    else{hgControls <- "No"}
    
    content <- 
      tagList(
        tags$h4(selectedFac$facilityName[1]),
        tags$strong("Facility ID:"),sprintf(" %s", as.character(facilityId)), tags$br(),
        tags$strong("State:"),sprintf(" %s", selectedFac$stateName[1]), tags$br(),
        tags$h5(tags$u("Primary Fuel Type")),
        sprintf("%s", fuelTypesStg), tags$br(),
        tags$h5(tags$u("Pollutant Controls")),
        tags$strong("SO2 Controls Installed:"),sprintf("%s", so2Controls), tags$br(),
        tags$strong("NOx Controls Installed:"),sprintf("%s", noxControls), tags$br(),
        tags$strong("Particulate Matter Controls Installed:"),sprintf("%s", pmControls), tags$br(),
        tags$strong("Mercury Controls Installed:"),sprintf("%s", hgControls), tags$br(), 
        div(style="margin-top:5px;","For more information on pollutants, please visit:"),
        tags$a(href="https://www.epa.gov/criteria-air-pollutants", 
               "https://www.epa.gov/criteria-air-pollutants",
               target="_blank"),
        tags$a(href="https://www.epa.gov/mercury", 
               "https://www.epa.gov/mercury",
               target="_blank"),
        tags$h5(tags$u("Unit Operating Statuses:")),
        HTML(operatingStatuses)
      )
  }
  
  get_compliance_info_for_side_panel <- function(facilityId){
    
    selectedFac <- unitData[unitData$facilityId == facilityId,]
    complianceFacilityData <- get_allow_comp_data(latestComplianceYear,facilities=c(facilityId))
    
    accountNumber <- as.character(complianceFacilityData$accountNumber[1])
    if (length(accountNumber) == 0){
      accountNumber <- "No account associated with this facility."
    }
    
    if(is.null(complianceFacilityData)){
      subjectedPrograms <- "This facility is not subjected to CAMD's Allowance-based programs."
      complianceDisplayTable <- ""
      outOfComplianceTable <- ""
      }
    else{
      subjectedPrograms <- paste0(unique(complianceFacilityData$programCodeInfo),collapse = ", ")
      if (subjectedPrograms == ""){
        subjectedPrograms <- "This facility is not subjected to CAMD's Allowance-based programs."
      }
      # Consider printing new compliance data (e.g. when ARP has data before CSAPR)
      'complianceTable <- complianceFacilityData[c("programCodeInfo",
                                                  "allocated",
                                                  "totalAllowancesHeld",
                                                  "complianceYearEmissions",
                                                  "carriedOver")]
      colnames(complianceTable) <- c("Program","Allocated","Held","Emissions","Banked")
      complianceDisplayTable <- tagList(
        tags$h5("Compliance for 2020:"),
        HTML(getHTML(complianceTable))
      )'
      complianceFor2020 <- complianceFacilityData[complianceFacilityData$year == latestComplianceYear,]
      
      compTableForLatestYear <- bind_rows(lapply(complianceFor2020$programCodeInfo, function(prg){
        if (is.na(complianceFor2020$excessEmissions[complianceFor2020$programCodeInfo == prg])){
          compStr <- "Yes"
        }
        else{compStr <- "No"}
        c("Program"=prg, "In compliance?"=compStr)
      }))
      
      complianceDisplayTable <- tagList(
        tags$h5(tags$u(paste0("Compliance for ",latestComplianceYear,":"))),
        HTML(getHTML(compTableForLatestYear))
      )
      
      compYearsOutOfComp <- bind_rows(lapply(1:nrow(complianceFacilityData), function(row){
        if (!is.na(complianceFacilityData[row,"excessEmissions"])){
          c("Program"=complianceFacilityData$programCodeInfo[row], 
            "Year"=complianceFacilityData$year[row],
            "In compliance?"="No")
        }
      }))
      
      if(nrow(compYearsOutOfComp) == 0){
        outOfComplianceTable <- HTML("<br>This facility has no record of being out of compliance.")
      }
      else{
        outOfComplianceTable <- tagList(
          tags$h5(tags$u("Compliance from past years:")),
          HTML(getHTML(outOfComplianceTable))
        )
      }
      
      'beginDate <- paste0(latestComplianceYear,"-01-01")
      endDate <- paste0(latestComplianceYear,"-12-31")
      transactionData <- get_transaction_data(facilityId,beginDate,endDate)
      if(is.null(transactionData)){
        transactionDisplayTable <- ""
      }
      else{
        transactionTableData <- make_transaction_table(transactionData,facilityId)
        transactionDisplayTable <- tagList(
          tags$h5("Transaction data for 2020:"),
          HTML(getHTML(transactionTableData))
        )
        }'
      
      }
    
    content <- 
      tagList(
        tags$h4(selectedFac$facilityName[1]),
        tags$strong("Account Number:"),sprintf(" %s", accountNumber),
        tags$h5(tags$u("Subjected Programs:")),sprintf(" %s", subjectedPrograms), tags$br(),
        complianceDisplayTable,
        outOfComplianceTable
      )
    
    content
    
  }
  
}


