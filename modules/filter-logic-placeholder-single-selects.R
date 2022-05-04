
columnFilterPlaceholderSingleSelectUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filter_container"))
}

columnFilterPlaceholderSingleSelect <- function(input, output, session, df, 
                                                col_num, choice_filter, 
                                                clearEvent, columnNumberIndex,
                                                facilityMap) {
  
  # This renders a selectInput and only re-renders when the selected data
  # frame changes. (i.e. it doesn't re-render when filters change state.)
  output$filter_container <- renderUI({
    # Don't render if col_num is > actual number of cols
    req(col_num <= ncol(df()))
    
    # Labels are coming from labelConversion in global
    if (facilityMap){
      label <- facilityMapLabelConversion$label[facilityMapLabelConversion$columnName == names(df())[[col_num]]]
      placeholder <- facilityMapLabelConversion$placeholder[facilityMapLabelConversion$columnName == names(df())[[col_num]]]
    }
    else{
      label <- singleLabelConversion$label[singleLabelConversion$columnName == names(df())[[col_num]]]
      placeholder <- singleLabelConversion$placeholder[singleLabelConversion$columnName == names(df())[[col_num]]]
      label <- paste0(columnNumberIndex,". ",label)
    }
    
    freezeReactiveValue(input, "filter_value")
    
    selectizeInput(session$ns("filter_value"), label,
                   choices = NULL,
                   multiple = FALSE, options = list(
                     placeholder = placeholder))
    
  })
  
  # 
  observeEvent(clearEvent(),{
    updateSelectizeInput(session, "filter_value",
                         choices = sort(unique(df()[,col_num,drop=TRUE])),
                         selected = character(0),
                         server=TRUE)
    
  })
  
  # When the other filters change, update this filter to remove rows that
  # are filtered out by the other filters' criteria. (We also add in the
  # currently selected values for this filter, so that changing other
  # filters does not cause this filter's selected values to be unselected;
  # while that behavior might make sense logically, it's a poor user
  # experience.)
  
  observeEvent(choice_filter(), {
    current_values <- input$filter_value
    updateSelectizeInput(session, "filter_value",
                         choices = sort(unique(c(current_values, df()[choice_filter(),col_num,drop=TRUE]))),
                         selected = current_values)
    
  }, ignoreInit = TRUE)
  
  return(reactive({input$filter_value}))
}

columnFilterPlaceholderSingleSelectSetUI <- function(id, column_indexes, column_width) {
  ns <- NS(id)
  lapply(as.list(column_indexes), function(i) {
    column(column_width,
           columnFilterPlaceholderSingleSelectUI(ns(paste0("col", i)))
    )
  })
}

rowFilterPlaceholderSingleSelectSetUI <- function(id, column_indexes) {
  ns <- NS(id)
  lapply(as.list(column_indexes), function(i) {
    columnFilterPlaceholderSingleSelectUI(ns(paste0("col", i)))
  })
}

columnFilterPlaceholderSingleSelectSet <- function(input, output, session, 
                                                   df, column_indexes, clearEvent,
                                                   facilityMap=FALSE) {
  
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
    
    columnNumberIndex <- match(i,column_indexes)
    
    filter_values <- callModule(columnFilterPlaceholderSingleSelect, paste0("col", i), df, i, 
                                create_choice_filter(which(column_indexes==i)),
                                clearEvent, columnNumberIndex,facilityMap)
    
    # Return a reactive that is a row index of selected rows, according to
    # just this filter. If this filter shouldn't be taken into account
    # because its i is too high, or if there are no values selected,
    # just return TRUE to accept all rows.
    reactive({
      if (i > ncol(df())) {
        TRUE
      } else if (!isTruthy(filter_values())) {
        toReturn$selections[[names(df())[[i]]]] <- filter_values()
        TRUE
      } else if (filter_values() == "Select All") {
        toReturn$selections[[names(df())[[i]]]] <- filter_values()
        TRUE
      } else {
        toReturn$selections[[names(df())[[i]]]] <- filter_values()
        df()[,i,drop=TRUE] %in% filter_values()
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