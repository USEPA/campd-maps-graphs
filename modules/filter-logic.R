
columnFilterUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filter_container"))
}

columnFilter <- function(input, output, session, df, 
                         col_num, choice_filter,
                         clearEvent, columnNumberIndex, 
                         programIsSingleSelect) {
  # This renders a selectInput and only re-renders when the selected data
  # frame changes. (i.e. it doesn't re-render when filters change state.)
  output$filter_container <- renderUI({
    # Don't render if col_num is > actual number of cols
    req(col_num <= ncol(df()))
    
    # Labels are coming from labelConversion in global
    if (programIsSingleSelect & (names(df())[[col_num]] %in% c("programDescription","programCode"))){
      label <- singleLabelConversion$label[singleLabelConversion$columnName == "programDescription"]
      placeholder <- singleLabelConversion$placeholder[singleLabelConversion$columnName == "programDescription"]
    }
    else{
      label <- mulitLabelConversion$label[mulitLabelConversion$columnName == names(df())[[col_num]]]
    }
    label <- paste0(columnNumberIndex,". ",label)
    
    freezeReactiveValue(input, "filter_value")
    if (names(df())[[col_num]] == "year"){
      years <- unique(df()[[col_num]])
      sliderInput(session$ns("filter_value"), label,
                  min = min(years), max = max(years),
                  value = c(min(years),max(years)),step = 1,sep = "")
    }
    else if (programIsSingleSelect & (names(df())[[col_num]] %in% c("programDescription","programCode"))){
      selectizeInput(session$ns("filter_value"), label,
                     choices = NULL,
                     multiple = FALSE, options = list(
                       placeholder = placeholder))
      
    }
    else{
      selectizeInput(session$ns("filter_value"), label,
                  choices = sort(unique(df()[,col_num,drop=TRUE])),
                  multiple = TRUE, options = list(
                    'plugins' = list('remove_button'),
                    'create' = TRUE,
                    'persist' = FALSE,
                    maxItems = 5))
    }
    
  })
  
  observe({
    if (programIsSingleSelect & (names(df())[[col_num]] %in% c("programDescription","programCode"))){
      updateSelectizeInput(session, "filter_value",
                           choices = sort(unique(df()[,col_num,drop=TRUE])),
                           selected = character(0),
                           server=FALSE)
      
    }
  })
  
  observeEvent(clearEvent(),{
    if (names(df())[[col_num]] == "year"){
      years <- unique(df()[[col_num]])
      updateSliderInput(session, "filter_value",
                  min = min(years), max = max(years),
                  value = c(min(years),max(years)))
    }
    else if (programIsSingleSelect & (names(df())[[col_num]] %in% c("programDescription","programCode"))){
      updateSelectizeInput(session, "filter_value",
                           choices = sort(unique(df()[,col_num,drop=TRUE])),
                           selected = character(0),
                           server=FALSE)
    }
    else{
      updateSelectizeInput(session, "filter_value",
                           choices = sort(unique(df()[,col_num,drop=TRUE])),
                           selected = NULL
      )
    }
    
  },ignoreInit = TRUE)
  
  # When the other filters change, update this filter to remove rows that
  # are filtered out by the other filters' criteria. (We also add in the
  # currently selected values for this filter, so that changing other
  # filters does not cause this filter's selected values to be unselected;
  # while that behavior might make sense logically, it's a poor user
  # experience.)
  observeEvent(choice_filter(), {
    current_values <- input$filter_value
    if (names(df())[[col_num]] == "year"){
      updateSliderInput(session, "filter_value",
                        min = min(sort(unique(c(df()[choice_filter(),col_num,drop=TRUE])))),
                        max = max(sort(unique(c(df()[choice_filter(),col_num,drop=TRUE]))))
                        )
    }
    else{
      if (programIsSingleSelect & (names(df())[[col_num]] %in% c("programDescription","programCode"))){
        updateSelectizeInput(session, "filter_value",
                       choices = sort(unique(c(current_values, df()[choice_filter(),col_num,drop=TRUE]))),
                       selected = current_values)
      }
      else{
        updateSelectizeInput(session, "filter_value",
                        choices = sort(unique(c(current_values, df()[choice_filter(),col_num,drop=TRUE]))),
                        selected = current_values
                        )
      }
    }
  })
  
  return(reactive({input$filter_value}))
}

columnFilterSetUI <- function(id, column_indexes) {
  ns <- NS(id)
  lapply(as.list(column_indexes), function(i) {
    columnFilterUI(ns(paste0("col", i)))
  })
}

columnFilterSet <- function(input, output, session, 
                            df, column_indexes, clearEvent,
                            startIndexAdd=0,
                            programIsSingleSelect=FALSE) {
  
  toReturn <- reactiveValues(data = df, selections = list(), col=list())
  
  # Each column filter needs to only display the choices that are
  # permitted after all the OTHER filters have had their say. But
  # each column filter must not take its own filter into account
  # (hence we do filter[-col], not filter, in the reactive below).
  create_choice_filter <- function(col) {
    reactive({
      filter_values <- lapply(filters[-col], do.call, args = list())
      Reduce(`&`, filter_values, TRUE)
    })
  }
  
  # filters is a list of reactive expressions, each of which is a
  # logical vector of rows to be selected.
  filters <- lapply(column_indexes, function(i) {
    
    columnNumberIndex <- match(i,column_indexes)+startIndexAdd
    
    filter_values <- callModule(columnFilter, paste0("col", i), df, i, 
                                create_choice_filter(which(column_indexes==i)),
                                clearEvent, columnNumberIndex, 
                                programIsSingleSelect)
    # Return a reactive that is a row index of selected rows, according to
    # just this filter. If this filter shouldn't be taken into account
    # because its col_num is too high, or if there are no values selected,
    # just return TRUE to accept all rows.
    reactive({
      if (i > ncol(df())) {
        TRUE
      } else if (!isTruthy(filter_values())) {
        toReturn$selections[[names(df())[[i]]]] <- filter_values()
        TRUE
      } else {
        if (names(df())[[i]] == "year"){
          toReturn$selections[[names(df())[[i]]]] <- seq(filter_values()[1],filter_values()[2])
          df()[,i,drop=TRUE] %in% seq(filter_values()[1],filter_values()[2])
        }
        else{
          toReturn$selections[[names(df())[[i]]]] <- filter_values()
          df()[,i,drop=TRUE] %in% filter_values()
        }
      }
    })
  })
  
  observe({
    # Unpack the list of reactive expressions to a list of logical vectors
    filter_values <- lapply(filters, do.call, args = list())
    # Combine all the logical vectors using & operator
    selected_rows <- Reduce(`&`, filter_values, TRUE)
    # Store the data frame, filtered by the selected rows
    toReturn$data <- df()[selected_rows,]
  })
  
  return(toReturn)
}