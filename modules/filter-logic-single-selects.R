
columnFilterSingleSelectUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("filter_container"))
}

columnFilterSingleSelect <- function(input, output, session, df, 
                                     col_num, choice_filter, isRequired) {
  # This renders a selectInput and only re-renders when the selected data
  # frame changes. (i.e. it doesn't re-render when filters change state.)
  output$filter_container <- renderUI({
    # Don't render if col_num is > actual number of cols
    req(col_num <= ncol(df()))
    
    # Labels are coming from labelConversion in global
    label <- singleLabelConversion$label[singleLabelConversion$columnName == names(df())[[col_num]]]
    
    freezeReactiveValue(input, "filter_value")
    
    if (isRequired){
      selectizeInput(session$ns("filter_value"), label,
                     choices = sort(unique(df()[,col_num,drop=TRUE])),
                     selected = sort(unique(df()[,col_num,drop=TRUE]))[1],
                     multiple = FALSE)
    }
    
    else{
      selectizeInput(session$ns("filter_value"), label,
                   choices = append(c("Select All"),sort(unique(df()[,col_num,drop=TRUE]))),
                   selected = c("Select All"),
                   multiple = FALSE)
    }
    
  })
  
  # When the other filters change, update this filter to remove rows that
  # are filtered out by the other filters' criteria. (We also add in the
  # currently selected values for this filter, so that changing other
  # filters does not cause this filter's selected values to be unselected;
  # while that behavior might make sense logically, it's a poor user
  # experience.)
  observeEvent(choice_filter(), {
    current_values <- input$filter_value
    if (isRequired){
      updateSelectizeInput(session, "filter_value",
                           choices = sort(unique(c(current_values, df()[choice_filter(),col_num,drop=TRUE]))),
                           selected = current_values)
    }
    else {
      updateSelectizeInput(session, "filter_value",
                         choices = append(c("Select All"),sort(unique(c(current_values, df()[choice_filter(),col_num,drop=TRUE])))),
                         selected = current_values)
    }
  })
  
  return(reactive({input$filter_value}))
}

columnFilterSingleSelectSetUI <- function(id, column_indexes) {
  ns <- NS(id)
  lapply(as.list(column_indexes), function(i) {
    column(3,
           columnFilterSingleSelectUI(ns(paste0("col", i)))
    )
  })
}

rowFilterSingleSelectSetUI <- function(id, column_indexes) {
  ns <- NS(id)
  lapply(as.list(column_indexes), function(i) {
    columnFilterSingleSelectUI(ns(paste0("col", i)))
  })
}

columnFilterSingleSelectSet <- function(input, output, session, 
                            df, column_indexes, isRequired=FALSE) {
  
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
    filter_values <- callModule(columnFilterSingleSelect, paste0("col", i), df, i, 
                                create_choice_filter(which(column_indexes==i)),
                                isRequired)
    
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